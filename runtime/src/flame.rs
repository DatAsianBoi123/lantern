use std::{cell::OnceCell, fmt::Formatter, hash, ops::ControlFlow};

use arena::Arena;
use diagnostic::{Diagnostic, DiagnosticSink, error, symbol::{Symbol, SymbolDisplay, SymbolTable}};
use instruction::InstructionSet;
use parse::{FunArg, IfBranch, IfStmt, Item, ItemFun, ItemNativeFun, ItemPrimitive, ItemStruct, LanternFile, ReturnStmt, Stmt, StructField, ValDeclaration, WhileStmt, expr::{BinaryOperator, Expr, ExprArray, ExprBinary, ExprBlock, ExprField, ExprFunCall, ExprIndex, ExprParen, ExprStruct, ExprUnary, UnaryOperator}, lex::{Break, Ident, Literal, TokenKind}};

use crate::{Slot, VM, error::RuntimeError, flame::{instruction::Instruction, scope::{Globals, LineMap, LoopContext, LoopScope, Scope, ScopeKind, StackFrame}, r#type::{LanternType, TypeContext, TypeId}}, heap::{HeapObject, ObjectHeader, TypeInfo}, inst};

pub type NativeFn = fn(&mut VM) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod r#type;
pub mod scope;
pub mod native;

pub fn ignite(file: LanternFile, globals: &mut Globals, sink: &mut DiagnosticSink, symbol_table: &SymbolTable) -> GeneratedFunction {
    let mut r#gen = FlameGen::new(globals, sink, symbol_table);
    let arena = Arena::new(25);
    let tcx = TypeContext::new(&arena);
    let _ = r#gen.compile_stmts(file.stmts, Scope::new(), &tcx);
    r#gen.frame.into_gen()
}

#[derive(Debug)]
pub struct FlameGen<'a, 't> {
    pub frame: StackFrame<'t>,
    pub globals: &'a mut Globals,
    pub sink: &'a mut DiagnosticSink,
    pub symbol_table: &'a SymbolTable<'a>,
    loop_context: LoopContext,
}

impl<'a, 't> FlameGen<'a, 't> {
    pub fn new(globals: &'a mut Globals, sink: &'a mut DiagnosticSink, symbol_table: &'a SymbolTable) -> Self {
        Self {
            frame: StackFrame::new_module(),
            globals,
            sink,
            symbol_table,
            loop_context: LoopContext::new(),
        }
    }

    pub fn using_frame<F: FnOnce(&mut Self)>(&mut self, mut frame: StackFrame<'t>, fun: F) -> GeneratedFunction {
        std::mem::swap(&mut self.frame, &mut frame);
        fun(self);
        std::mem::swap(&mut self.frame, &mut frame);
        frame.into_gen()
    }

    // use a &TypeContext to tell the compiler that tcx is not borrowed mutably until partial
    // borrows of `self` are allowed
    pub fn compile_stmts(&mut self, statements: Vec<Stmt>, mut scope: Scope<'_, 't>, tcx: &TypeContext<'t>) -> ControlFlow<()> {
        self.resolve_types(&statements, &mut scope, tcx);

        statements.iter()
            .filter_map(|statement| {
                if let Stmt::Item(item) = statement {
                    Some(item)
                } else {
                    None
                }
            })
            .for_each(|item| {
                match item {
                    Item::Using(_) => {},
                    Item::Fun(ItemFun { path, args, ret, .. }) => {
                        let args = args.iter()
                            .map(|FunArg { ident, r#type, .. }| (ident.clone(), self.sink.emit_or(LanternType::resolve(r#type, &scope, tcx), tcx.null())))
                            .collect();

                        let ret = ret.as_ref()
                            .map(|(_, r#type)| self.sink.emit_or(LanternType::resolve(r#type, &scope, tcx), tcx.null()))
                            .unwrap_or(tcx.null());

                        let name = path.last().0;
                        let fun = LanternFunction::new(self.globals.funs.len(), args, ret, tcx);
                        if path.items.len() == 1 {
                            if scope.insert_function(name, fun).is_none() {
                                error!(in self.sink; path.last().span() => "function already declared");
                            }
                        } else {
                            let ident = &path.items[0];
                            if let Some(item) = scope.item(ident.0) {
                                if scope.insert_associated(item, name, fun).is_none() {
                                    error!(in self.sink; ident.span() => "associated function already declared");
                                }
                            } else {
                                error!(in self.sink; ident.span() => "item {} not found", self.display(ident));
                            }
                        }
                        // this gets overridden when the function is generated
                        self.globals.funs.push(GeneratedFunction::new("".into(), FunctionKind::Native(native::dummy_native)));
                    },
                    Item::NativeFun(ItemNativeFun { ident, args, ret, .. }) => {
                        let args = args.iter()
                            .map(|FunArg { ident, r#type, .. }| (ident.clone(), self.sink.emit_or(LanternType::resolve(r#type, &scope, tcx), tcx.null())))
                            .collect();

                        let ret = ret.as_ref()
                            .map(|(_, r#type)| self.sink.emit_or(LanternType::resolve(r#type, &scope, tcx), tcx.null()))
                            .unwrap_or(tcx.null());

                        scope.insert_function(ident.0, LanternFunction::new(self.globals.funs.len(), args, ret, tcx));

                        let ptr = native::get_native_fn(self.symbol_table.resolve(ident.0)).unwrap_or_else(|| {
                            error!(in self.sink; ident.span() => "unknown native `{}`", self.display(ident));
                            native::dummy_native
                        });

                        self.globals.funs.push(GeneratedFunction::new(self.symbol_table.resolve(ident.0).into(), FunctionKind::Native(ptr)));
                    },
                    Item::Struct(ItemStruct { ident, fields, .. }) => {
                        let fields = fields.iter()
                            .map(|StructField { ident, r#type, .. }| {
                                // type may not have fields initialized, but structs have static
                                // size/alignment and primitives are hardcoded
                                (ident.0, self.sink.emit_or(LanternType::resolve(r#type, &scope, tcx), tcx.null()))
                            })
                            .collect();

                        let item = scope.item(ident.0).expect("types were resolved");
                        let LanternType::Struct(ref r#struct) = *item else { panic!("resolved type not a struct") };
                        r#struct.init(fields);
                        self.globals.types[r#struct.id] = r#struct.to_type_info();
                    },
                    Item::Primitive(_) => {},
                }
            });

        for statement in statements {
            match statement {
                Stmt::IfStmt(if_stmt) => {
                    let mut end_indices = Vec::new();
                    let mut current_branch = IfBranch::ElseIf(if_stmt);
                    let mut overall_return = None;
                    let mut has_else = false;

                    loop {
                        match current_branch {
                            IfBranch::ElseIf(IfStmt { condition, block, branch, .. }) => {
                                let condition_span = condition.span();
                                let ty = self.compile_expr(condition, &scope, tcx)?;
                                if ty != tcx.primitive(&native::BOOL_PRIMITIVE) {
                                    error!(in self.sink; condition_span => "expected `bool`, but got {} instead", self.display(&ty));
                                }

                                let false_index = self.frame.instructions.len();
                                inst!(with self.frame => block.open_brace.span(); GOTO_IF_FALSE 0);

                                let block_scope = scope.child_block();
                                let branch_return = self.compile_stmts(block.stmts, block_scope, tcx);
                                match (overall_return, branch_return) {
                                    (Some(ControlFlow::Break(_)), ControlFlow::Break(_)) => {},
                                    (Some(ControlFlow::Break(_)), ControlFlow::Continue(_)) => overall_return = Some(branch_return),
                                    (Some(ControlFlow::Continue(_)), _) => {},
                                    (None, _) => overall_return = Some(branch_return),
                                }
                                end_indices.push(self.frame.instructions.len());
                                inst!(with self.frame => block.closed_brace.span(); GOTO 0);
                                self.frame.instructions[false_index] = Instruction::PopGotoIfFalse(self.frame.instructions.len());

                                match branch {
                                    Some((_, next_branch)) => current_branch = *next_branch,
                                    None => break,
                                }
                            },
                            IfBranch::Else(block) => {
                                let block_scope = scope.child_block();
                                let branch_return = self.compile_stmts(block.stmts, block_scope, tcx);
                                match (overall_return, branch_return) {
                                    (Some(ControlFlow::Break(_)), ControlFlow::Break(_)) => {},
                                    (Some(ControlFlow::Break(_)), ControlFlow::Continue(_)) => overall_return = Some(branch_return),
                                    (Some(ControlFlow::Continue(_)), _) => {},
                                    (None, _) => overall_return = Some(branch_return),
                                }
                                has_else = true;
                                break;
                            },
                        }
                    }

                    for index in end_indices {
                        self.frame.instructions[index] = Instruction::Goto(self.frame.instructions.len());
                    }

                    if let Some(ControlFlow::Break(r#type)) = overall_return && has_else {
                        return ControlFlow::Break(r#type);
                    }
                },
                Stmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                    let condition_span = condition.span();
                    let head = self.frame.instructions.len();

                    let ty = self.compile_expr(condition, &scope, tcx)?;
                    if ty != tcx.primitive(&native::BOOL_PRIMITIVE) {
                        error!(in self.sink; condition_span => "expected `bool`, but got {} instead", self.display(&ty));
                    }
                    let condition_index = self.frame.instructions.len();
                    inst!(with self.frame => block.open_brace.span(); POP_GOTO_IF_FALSE 0);

                    self.loop_context.scopes.push(LoopScope::new(head));
                    let block_scope = scope.child_block();
                    // we can't assume the initial condition is met so these may not even be ran
                    let _ = self.compile_stmts(block.stmts, block_scope, tcx);
                    inst!(with self.frame => block.closed_brace.span(); GOTO head);

                    self.frame.instructions[condition_index] = Instruction::PopGotoIfFalse(self.frame.instructions.len());

                    for break_index in self.loop_context.scopes.pop().expect("in loop").breaks {
                        self.frame.instructions[break_index] = Instruction::Goto(self.frame.instructions.len());
                    }
                },
                Stmt::ValDeclaration(ValDeclaration { val, ident, r#type, init: None, .. }) => {
                    // TODO: unitialized vars
                    let local_index = self.frame.declare_local();
                    let ty = r#type
                        .ok_or(error!(val.span() => "explicit type required on an initialized variable"))
                        .and_then(|(_, r#type)| LanternType::resolve(&r#type, &scope, tcx));
                    if scope.insert_variable(ident.0, local_index, self.sink.emit_or(ty, tcx.null())).is_none() {
                        error!(in self.sink; ident.span() => "variable `{}` already declared", self.display(&ident));
                    }
                    inst! { with self.frame => val.span();
                        [PUSHU 0]
                        [STORE_LOCAL local_index]
                        [POP]
                    }
                },
                Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                    let init_span = init.span();
                    let init_type = self.compile_expr(init, &scope, tcx)?;

                    let var_type = match r#type {
                        Some((_, r#type)) => {
                            let var_type = self.sink.emit_or(LanternType::resolve(&r#type, &scope, tcx), tcx.null());
                            if var_type != init_type {
                                error!(in self.sink; init_span => "expected {}, but got {} instead", self.display(&var_type), self.display(&init_type));
                            }
                            var_type
                        },
                        None => init_type,
                    };
                    let local_index = self.frame.declare_local();
                    if scope.insert_variable(ident.0, local_index, var_type).is_none() {
                        error!(in self.sink; ident.span() => "variable `{}` already declared", self.display(&ident));
                    }
                    inst! { self.frame.instructions;
                        [STORE_LOCAL local_index]
                        [POP]
                    };
                },
                Stmt::Return(ReturnStmt { ret: ret_keyword, expr, .. }) => {
                    let Some(expected_ret) = self.frame.ret_type else {
                        error!(in self.sink; ret_keyword.span() => "{ret_keyword} not allowed here");
                        return ControlFlow::Break(());
                    };
                    let ret = if let Some(expr) = expr {
                        self.compile_expr(expr, &scope, tcx)?
                    } else {
                        inst!(with self.frame => ret_keyword.span(); PUSHU 0);
                        tcx.null()
                    };
                    if expected_ret != ret {
                        error!(in self.sink; ret_keyword.span() => "expected {}, but got {} instead", self.display(&expected_ret), self.display(&ret));
                    }
                    inst!(self.frame.instructions; RET);
                    return ControlFlow::Break(());
                },
                Stmt::Continue(continue_keyword, _) => {
                    if let Some(LoopScope { head, .. }) = self.loop_context.scopes.last() {
                        inst!(with self.frame => continue_keyword.span(); GOTO *head);
                    } else {
                        error!(in self.sink; continue_keyword.span() => "{continue_keyword} not allowed here");
                    }
                },
                Stmt::Break(Break(span), _) => {
                    if let Some(LoopScope { breaks, .. }) = self.loop_context.scopes.last_mut() {
                        breaks.push(self.frame.instructions.len());
                        inst!(with self.frame => span; GOTO 0);
                    } else {
                        error!(in self.sink; span => "`break` not allowed here");
                    }
                },
                Stmt::Throw(_, expr, semi) => {
                    let span = expr.span();
                    let ty = self.compile_expr(expr, &scope, tcx)?;
                    // TODO: string type
                    let byte = tcx.primitive(&native::BYTE_PRIMITIVE);
                    if ty != tcx.intern(LanternType::Array(byte)) {
                        error!(in self.sink; span => "expected `[u8]`, but got {} instead", self.display(&ty));
                    }
                    inst!(with self.frame => semi.span(); THRW);
                },
                Stmt::Expr(expr, _) => {
                    self.compile_expr(expr, &scope, tcx)?;
                    inst!(self.frame.instructions; POP);
                },
                Stmt::Item(Item::Fun(ItemFun { path, block, ret, .. })) => {
                    let ret = ret
                        .map(|(_, r#type)| self.sink.emit_or(LanternType::resolve(&r#type, &scope, tcx), tcx.null()))
                        .unwrap_or(tcx.null());

                    let fun = if path.items.len() == 1 {
                        scope.function(path.last().0).expect("function in scope")
                    } else {
                        scope.associated(scope.item(path.items[0].0).expect("item in scope"), path.last().0).expect("assosiated in scope")
                    };

                    let mut fun_scope = scope.child_function(block.closed_brace.span());
                    let mut fun_frame = StackFrame::new_fun(self.display(&path), ret);

                    for (ident, ty) in &fun.args {
                        let local_index = fun_frame.declare_local();
                        if fun_scope.insert_variable(ident.0, local_index, *ty).is_none() {
                            error!(in self.sink; ident.span() => "argument `{}` already declared", self.display(ident));
                        }
                    }

                    let generated = self.using_frame(fun_frame, |nested| {
                        let _ = nested.compile_stmts(block.stmts, fun_scope, tcx);
                    });

                    self.globals.funs[fun.index] = generated;
                },
                Stmt::Item(Item::Using(_)) => {},
                Stmt::Item(Item::NativeFun(_)) => {},
                Stmt::Item(Item::Struct(_)) => {},
                Stmt::Item(Item::Primitive(_)) => {},
            }
        };

        match scope.into_kind() {
            // implicit return
            ScopeKind::Function(_, span) => {
                let ret_type = self.frame.ret_type.expect("function scope has return type");
                if ret_type != tcx.null() {
                    error!(in self.sink; span.clone() => "expected function to return null");
                };
                inst! { with self.frame => span;
                    [PUSHU 0]
                    [RET]
                }
                ControlFlow::Break(())
            },
            ScopeKind::Module => {
                // TODO: figure out what span to use
                inst! { self.frame.instructions;
                    [PUSHU 0]
                    [RET]
                }
                ControlFlow::Break(())
            },
            ScopeKind::Block(_) => ControlFlow::Continue(())
        }
    }

    pub fn compile_expr(&mut self, expression: Expr, scope: &Scope<'_, 't>, tcx: &TypeContext<'t>) -> ControlFlow<(), TypeId<'t>> {
        match expression {
            Expr::Literal(Literal::Integer(int, span)) => {
                inst!(with self.frame => span; PUSHI int);
                ControlFlow::Continue(tcx.primitive(&native::INT_PRIMITIVE))
            },
            Expr::Literal(Literal::Float(float, span)) => {
                inst!(with self.frame => span; PUSHF float);
                ControlFlow::Continue(tcx.primitive(&native::FLOAT_PRIMITIVE))
            },
            Expr::Literal(Literal::True(span)) => {
                inst!(with self.frame => span; PUSHU crate::bool_to_slot(true));
                ControlFlow::Continue(tcx.primitive(&native::BOOL_PRIMITIVE))
            },
            Expr::Literal(Literal::False(span)) => {
                inst!(with self.frame => span; PUSHU crate::bool_to_slot(false));
                ControlFlow::Continue(tcx.primitive(&native::BOOL_PRIMITIVE))
            },
            Expr::Literal(Literal::String(string, span)) => {
                // TODO: better string alloc
                inst!(with self.frame => span; ALLOC_STR string.clone());
                // TODO: make string a struct instead of array
                let byte = tcx.primitive(&native::BYTE_PRIMITIVE);
                ControlFlow::Continue(tcx.intern(LanternType::Array(byte)))
            },
            Expr::FunCall(ExprFunCall { expr, args, closed_paren, .. }) => {
                let span = expr.span();
                let ty = self.compile_expr(*expr, scope, tcx)?;
                if let LanternType::Function { is_method, args: ref fun_args, ret } = *ty {
                    let fun_args_len = fun_args.len();
                    if args.len() != fun_args.len() {
                        error!(in self.sink; span => "expected function to have {} args, got {} args instead", fun_args_len, args.len());
                    }

                    for (expr, ty) in args.into_iter().zip(fun_args) {
                        let expr_span = expr.span();
                        let expr_type = self.compile_expr(expr, scope, tcx)?;
                        if expr_type != *ty {
                            error!(in self.sink; expr_span => "expected {}, got {} instead", self.display(ty), self.display(&expr_type));
                        }
                    }

                    if is_method {
                        inst!(with self.frame => closed_paren.span(); INV_MET fun_args_len);
                    } else {
                        inst!(with self.frame => closed_paren.span(); INV fun_args_len);
                    }

                    ControlFlow::Continue(ret)
                } else {
                    error!(in self.sink; span => "expected function");
                    ControlFlow::Continue(tcx.null())
                }
            },
            Expr::Binary(ExprBinary { lhs, op, rhs }) => {
                // special cases
                match op {
                    BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                        let lhs = self.compile_expr(*lhs, scope, tcx)?;
                        let goto_index = self.frame.instructions.len();

                        match &op {
                            BinaryOperator::And(and) => inst!(with self.frame => and.span(); GOTO_IF_FALSE 0),
                            BinaryOperator::Or(or) => inst!(with self.frame => or.span(); GOTO_IF_TRUE 0),
                            _ => unreachable!(),
                        };
                        inst!(self.frame.instructions; POP);

                        let rhs = self.compile_expr(*rhs, scope, tcx)?;

                        let goto_inst = match op {
                            BinaryOperator::And(_) => Instruction::GotoIfFalse(self.frame.instructions.len()),
                            BinaryOperator::Or(_) => Instruction::GotoIfTrue(self.frame.instructions.len()),
                            _ => unreachable!(),
                        };
                        self.frame.instructions[goto_index] = goto_inst;

                        if !lhs.is_primitive_type(&native::BOOL_PRIMITIVE) || !rhs.is_primitive_type(&native::BOOL_PRIMITIVE) {
                            error!(in self.sink; op.span() => "{op} cannot be applied to {} and {}", self.display(&lhs), self.display(&rhs));
                        }

                        return ControlFlow::Continue(tcx.primitive(&native::BOOL_PRIMITIVE));
                    },
                    BinaryOperator::Assign(_) => {
                        match self.compile_lvalue(scope, tcx, *lhs)? {
                            Ok(lvalue) => {
                                let lhs = lvalue.write_type();
                                let rhs_span = rhs.span();
                                let rhs = self.compile_expr(*rhs, scope, tcx)?;
                                if lhs != rhs {
                                    error!(in self.sink; rhs_span => "expected {}, but got {} instead", self.display(&lhs), self.display(&rhs));
                                }

                                match lvalue {
                                    LValue::Local(var) => inst!(self.frame.instructions; STORE_LOCAL var.index),
                                    LValue::ArrayElement(_) => inst!(self.frame.instructions; WRITE_INDEX),
                                    LValue::StructField(ty) => inst!(self.frame.instructions; WRITE ty.size()),
                                }
                            },
                            Err(err) => self.sink.emit(err),
                        }
                        return ControlFlow::Continue(tcx.null());
                    },
                    BinaryOperator::AddAssign(_)
                    | BinaryOperator::SubAssign(_)
                    | BinaryOperator::MultAssign(_)
                    | BinaryOperator::DivAssign(_)
                    | BinaryOperator::ModAssign(_) => return self.compile_op_assign(scope, tcx, *lhs, op, *rhs),
                    _ => {},
                }

                let lhs = self.compile_expr(*lhs, scope, tcx)?;
                let rhs = self.compile_expr(*rhs, scope, tcx)?;

                if lhs != rhs {
                    error!(in self.sink; op.span() => "{op} cannot be applied to {} and {}", self.display(&lhs), self.display(&rhs));
                    return ControlFlow::Continue(tcx.null());
                }
                match (&*lhs, op, &*rhs) {
                    (LanternType::Primitive(lhs), op @ BinaryOperator::Neq(_), LanternType::Primitive(_)) if lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                        inst!(self.frame.instructions; NOT);
                        ControlFlow::Continue(tcx.primitive(&native::BOOL_PRIMITIVE))
                    },
                    (LanternType::Primitive(lhs), op, LanternType::Primitive(_)) if op.is_comparison() && lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                        ControlFlow::Continue(tcx.primitive(&native::BOOL_PRIMITIVE))
                    },
                    (LanternType::Primitive(lhs), op, LanternType::Primitive(_)) if lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                        ControlFlow::Continue(tcx.primitive(lhs))
                    },
                    (_, BinaryOperator::Assign(_) | BinaryOperator::And(_) | BinaryOperator::Or(_), _) => unreachable!(),
                    (lhs, op, rhs) => {
                        error!(in self.sink; op.span() => "{op} cannot be applied to {} and {}", self.display(lhs), self.display(rhs));
                        ControlFlow::Continue(tcx.null())
                    },
                }
            },
            Expr::Unary(ExprUnary { op, expr }) => {
                let ty = self.compile_expr(*expr, scope, tcx)?;
                match (op, &*ty) {
                    (op, LanternType::Primitive(primitive)) if primitive.ops.get_un_op(&op).is_some() => {
                        self.frame.instructions.push(primitive.ops.get_un_op(&op).unwrap());
                        ControlFlow::Continue(tcx.primitive(primitive))
                    },
                    (op, got) => {
                        error!(in self.sink; op.span() => "{op} cannot be applied to {}", self.display(got));
                        ControlFlow::Continue(tcx.null())
                    },
                }
            },
            Expr::Struct(ExprStruct { ident, fields, .. }) => {
                let Some(ty) = scope.item(ident.0) else {
                    error!(in self.sink; ident.span() => "unknown type");
                    return ControlFlow::Continue(tcx.null());
                };
                let LanternType::Struct(ref r#struct) = *ty else {
                    error!(in self.sink; ident.span() => "not a struct");
                    return ControlFlow::Continue(tcx.null());
                };
                inst!(with self.frame => ident.span(); ALLOC_OBJ r#struct.id);

                let mut init_fields = Vec::new();
                for field in fields {
                    match r#struct.find_field(field.ident.0) {
                        Some(struct_field) => {
                            let expr_span = field.expr.span();
                            inst!(with self.frame => field.ident.span(); PUSHU HeapObject::field_offset() + struct_field.offset);
                            let field_ty = self.compile_expr(field.expr, scope, tcx)?;
                            if field_ty != struct_field.ty {
                                error!(in self.sink; expr_span => "expected {}, but got {} instead", self.display(&struct_field.ty), self.display(&field_ty));
                            }
                            inst!(self.frame.instructions; WRITE struct_field.ty.size());
                            init_fields.push(field.ident);
                        },
                        None => error!(in self.sink; field.ident.span() => "unknown field `{}`", field.ident.display(self.symbol_table)),
                    }
                }

                if init_fields.len() != r#struct.data().fields.len() {
                    r#struct.data().fields.iter()
                        .filter(|field| !init_fields.iter().any(|ident| ident.0 == field.name))
                        .for_each(|field| error!(in self.sink; ident.span() => "missing field `{}`", self.symbol_table.resolve(field.name)));
                }

                ControlFlow::Continue(ty)
            },
            Expr::Paren(ExprParen { expr, .. }) => self.compile_expr(*expr, scope, tcx),
            Expr::Block(ExprBlock { stmts, closed_brace, .. }) => {
                let block_scope = scope.child_block();
                self.compile_stmts(stmts, block_scope, tcx)?;
                inst!(with self.frame => closed_brace.span(); PUSHU 0);
                ControlFlow::Continue(tcx.null())
            },
            Expr::Array(ExprArray { open_bracket, elements, closed_bracket, ty, .. }) => {
                let len = elements.len();
                let mut inner = ty.map(|ty| self.sink.emit_or(LanternType::resolve(&ty, scope, tcx), tcx.null()));

                for expr in elements {
                    let span = expr.span();
                    inner = match (inner, self.compile_expr(expr, scope, tcx)?) {
                        (None, ty) => Some(ty),
                        (Some(ty), expr_type) if ty == expr_type => Some(ty),
                        (Some(ty), expr_type) => {
                            error!(in self.sink; span => "expected {}, but got {} instead", self.display(&ty), self.display(&expr_type));
                            Some(ty)
                        },
                    }
                }
                let inner = inner.unwrap_or_else(|| {
                    error!(in self.sink; open_bracket.span() => "empty arrays require an explicit element type");
                    tcx.null()
                });
                if inner.is_ref() {
                    inst!(with self.frame => closed_bracket.span(); ALLOC_ARR VM::REF_ARR_TYPE_INDEX, len);
                } else {
                    inst!(with self.frame => closed_bracket.span(); ALLOC_ARR VM::PRIMITIVE_ARR_TYPE_INDEX, len);
                }
                ControlFlow::Continue(tcx.intern(LanternType::Array(inner)))
            },
            Expr::Index(ExprIndex { expr, index, closed_bracket, .. }) => {
                let expr_span = expr.span();
                let ty = self.compile_expr(*expr, scope, tcx)?;
                let inner = match *ty {
                    LanternType::Array(inner) => inner,
                    _ => {
                        error!(in self.sink; expr_span => "expected array or string");
                        tcx.null()
                    },
                };
                let index_span = index.span();
                let index_type = self.compile_expr(*index, scope, tcx)?;
                if index_type != tcx.primitive(&native::INT_PRIMITIVE) {
                    error!(in self.sink; index_span => "expected index to be an `int`");
                }

                inst!(with self.frame => closed_bracket.span(); INDEX);
                ControlFlow::Continue(inner)
            },
            Expr::Identifier(ident) => {
                let span = ident.span();
                if let Some(var) = scope.variable(ident.0) {
                    inst!(with self.frame => span; LOAD_LOCAL var.index);
                    ControlFlow::Continue(var.ty)
                } else if let Some(fun) = scope.function(ident.0) {
                    inst!(with self.frame => span; PUSHU fun.index);
                    ControlFlow::Continue(fun.assoc_type)
                } else {
                    error!(in self.sink; span => "unknown identifier `{}`", self.display(&ident));
                    ControlFlow::Continue(tcx.null())
                }
            },
            Expr::Field(ExprField { expr, ident }) => {
                // ex. Struct.static_fun
                if let Expr::Identifier(ref static_ident) = *expr && let Some(ty) = scope.item(static_ident.0) {
                    let Some(associated) = scope.associated(ty, ident.0) else {
                        error!(in self.sink; ident.span() => "unknown associated item `{}`", self.display(&ident));
                        return ControlFlow::Continue(tcx.null());
                    };
                    inst!(with self.frame => ident.span(); PUSHU associated.index);
                    return ControlFlow::Continue(associated.assoc_type);
                }

                let ty = self.compile_expr(*expr, scope, tcx)?;
                match *ty {
                    LanternType::Struct(ref r#struct) => {
                        if let Some(field) = r#struct.find_field(ident.0) {
                            let size = if field.ty.is_primitive() { field.ty.size() } else { 0 };
                            inst! { with self.frame => ident.span();
                                [PUSHU (HeapObject::field_offset() + field.offset)]
                                [READ size]
                            }
                            ControlFlow::Continue(field.ty)
                        } else if let Some(associated) = scope.associated(ty, ident.0) {
                            if associated.args.first().is_some_and(|(_, receiver)| *receiver == ty) {
                                inst!(with self.frame => ident.span(); PUSHU associated.index);
                            } else {
                                error!(in self.sink; ident.span() => "method must have a receiver");
                            }
                            ControlFlow::Continue(associated.method_type)
                        } else {
                            error!(in self.sink; ident.1 => "field {} does not exist in {}", self.display(&ident), self.display(&ty));
                            ControlFlow::Continue(tcx.null())
                        }
                    },
                    LanternType::Array(inner) => {
                        if self.symbol_table.resolve(ident.0) == "len" {
                            let size = if inner.is_primitive() { inner.size() } else { 0 };
                            inst! { with self.frame => ident.span();
                                [PUSHU size_of::<ObjectHeader>()]
                                [READ size]
                            }
                            ControlFlow::Continue(tcx.primitive(&native::INT_PRIMITIVE))
                        } else {
                            error!(in self.sink; ident.1 => "field {} does not exist in {}", self.display(&ident), self.display(&ty));
                            ControlFlow::Continue(tcx.null())
                        }
                    },
                    LanternType::Primitive(primitive) => {
                        if let Some(associated) = scope.associated(ty, ident.0) {
                            if associated.args.first().is_some_and(|(_, receiver)| *receiver == ty) {
                                inst!(with self.frame => ident.span(); PUSHU associated.index);
                            } else {
                                error!(in self.sink; ident.1 => "method must have a receiver");
                            }
                            ControlFlow::Continue(associated.method_type)
                        } else {
                            error!(in self.sink; ident.1 => "method {} does not exist in {}", self.display(&ident), primitive.name);
                            ControlFlow::Continue(tcx.null())
                        }
                    },
                    _ => {
                        error!(in self.sink; ident.1 => "field {} does not exist on {}", self.display(&ident), self.display(&ty));
                        ControlFlow::Continue(tcx.null())
                    }
                }
            },
        }
    }

    pub fn resolve_types(&mut self, statements: &[Stmt], scope: &mut Scope<'_, 't>, tcx: &TypeContext<'t>) {
        statements.iter()
            .filter_map(|statement| {
                if let Stmt::Item(item) = statement {
                    Some(item)
                } else {
                    None
                }
            })
            .for_each(|item| match item {
                Item::Using(_) => todo!(),
                Item::Struct(ItemStruct { ident, .. }) => {
                    let r#struct = LanternStruct::new(ident.0, self.globals.types.len());
                    // "dummy" typeinfo
                    self.globals.types.push(TypeInfo::Object { size: 0, ref_offets: Box::new([]) });
                    if scope.insert_item(ident.0, tcx.intern(LanternType::Struct(r#struct))).is_none() {
                        error!(in self.sink; ident.span() => "struct already declared");
                    }
                },
                Item::Primitive(ItemPrimitive { ident, .. }) => {
                    let Some(primitive) = native::get_primitive(self.symbol_table.resolve(ident.0)) else { panic!("unknown primitive `{}`", self.display(ident)) };
                    if scope.insert_item(ident.0, tcx.primitive(primitive)).is_none() {
                        error!(in self.sink; ident.span() => "primitive already declared");
                    }
                },
                _ => {},
            });
    }

    fn compile_lvalue(&mut self, scope: &Scope<'_, 't>, tcx: &TypeContext<'t>, lhs: Expr) -> ControlFlow<(), Result<LValue<'t>, Diagnostic>> {
        match lhs {
            Expr::Identifier(ident) => {
                ControlFlow::Continue(scope.variable(ident.0)
                    .map(LValue::Local)
                    .ok_or(error!(ident.span() => "unknown variable `{}`", self.display(&ident))))
            },
            Expr::Index(ExprIndex { expr, index, .. }) => {
                let expr_span = expr.span();
                let ty = self.compile_expr(*expr, scope, tcx)?;
                let inner = match *ty {
                    LanternType::Array(inner) => inner,
                    ref ty => {
                        error!(in self.sink; expr_span => "cannot index a {}", self.display(ty));
                        tcx.null()
                    },
                };

                let index_span = index.span();
                let index = self.compile_expr(*index, scope, tcx)?;
                if index != tcx.primitive(&native::INT_PRIMITIVE) {
                    error!(in self.sink; index_span => "expected index to be an int");
                }
                ControlFlow::Continue(Ok(LValue::ArrayElement(inner)))
            },
            Expr::Field(ExprField { expr, ident }) => {
                let expr_span = expr.span();
                match &*self.compile_expr(*expr, scope, tcx)? {
                    LanternType::Struct(r#struct) => {
                        let field_type = if let Some(field) = r#struct.find_field(ident.0) {
                            inst!(with self.frame => ident.span(); PUSHU (HeapObject::field_offset() + field.offset));
                            field.ty
                        } else {
                            error!(in self.sink; ident.span() => "field {} does not exist on {}", self.display(&ident), self.symbol_table.resolve(r#struct.name));
                            tcx.null()
                        };
                        ControlFlow::Continue(Ok(LValue::StructField(field_type)))
                    },
                    ty => {
                        error!(in self.sink; expr_span => "field {} does not exist on {}", self.display(&ident), self.display(ty));
                        ControlFlow::Continue(Ok(LValue::StructField(tcx.null())))
                    },
                }
            },
            _ => ControlFlow::Continue(Err(error!(lhs.span() => "bad left-hand-side of assignment"))),
        }
    }

    fn compile_op_assign(&mut self, scope: &Scope<'_, 't>, tcx: &TypeContext<'t>, lhs: Expr, op: BinaryOperator, rhs: Expr) -> ControlFlow<(), TypeId<'t>> {
        match lhs {
            Expr::Identifier(ident) => {
                let Some(var) = scope.variable(ident.0) else {
                    error!(in self.sink; ident.span() => "unknown variable `{}`", self.display(&ident));
                    return ControlFlow::Continue(tcx.null());
                };

                inst!(with self.frame => op.span(); LOAD_LOCAL var.index);

                let rhs = self.compile_expr(rhs, scope, tcx)?;
                if var.ty != rhs {
                    error!(in self.sink; op.span() => "{op} cannot be applied to {} and {}", self.display(&var.ty), self.display(&rhs));
                    return ControlFlow::Continue(tcx.null());
                }
                match (&*var.ty, &*rhs) {
                    (LanternType::Primitive(lhs), LanternType::Primitive(_)) if lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                    },
                    (lhs, rhs) => error!(in self.sink; op.span() => "{op} cannot be applied to {} and {}", self.display(lhs), self.display(rhs)),
                }
                inst!(self.frame.instructions; STORE_LOCAL var.index);
            },
            Expr::Index(_) => todo!("operator assignment is currently only supported on locals"),
            Expr::Field(_) => todo!("operator assignment is currently only supported on locals"),
            _ => error!(in self.sink; op.span() => "bad left-hand-side of assignment"),
        }
        ControlFlow::Continue(tcx.null())
    }

    fn display<T: SymbolDisplay>(&self, dis: &T) -> String {
        dis.display(self.symbol_table)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LValue<'t> {
    Local(LanternVariable<'t>),
    ArrayElement(TypeId<'t>),
    StructField(TypeId<'t>),
}

impl<'t> LValue<'t> {
    pub fn write_type(self) -> TypeId<'t> {
        match self {
            Self::Local(var) => var.ty,
            Self::ArrayElement(ty) => ty,
            Self::StructField(ty) => ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternFunction<'t> {
    pub index: usize,
    pub args: Vec<(Ident, TypeId<'t>)>,
    pub ret: TypeId<'t>,
    pub assoc_type: TypeId<'t>,
    pub method_type: TypeId<'t>,
}

impl<'t> LanternFunction<'t> {
    pub fn new(index: usize, args: Vec<(Ident, TypeId<'t>)>, ret: TypeId<'t>, tcx: &TypeContext<'t>) -> Self {
        Self {
            assoc_type: tcx.intern(Self::to_assoc_type(&args, ret)),
            method_type: tcx.intern(Self::to_method_type(&args, ret)),
            index,
            args,
            ret,
        }
    }

    fn to_assoc_type(args: &[(Ident, TypeId<'t>)], ret: TypeId<'t>) -> LanternType<'t> {
        LanternType::Function { is_method: false, args: args.iter().map(|(_, ty)| *ty).collect(), ret }
    }

    fn to_method_type(args: &[(Ident, TypeId<'t>)], ret: TypeId<'t>) -> LanternType<'t> {
        // first arg is assumed to be the receiver
        LanternType::Function { is_method: true, args: args.iter().skip(1).map(|(_, ty)| *ty).collect(), ret }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStruct<'t> {
    pub name: Symbol,
    pub id: usize,
    pub data: OnceCell<LanternStructData<'t>>,
}

impl hash::Hash for LanternStruct<'_> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.id);
    }
}

impl<'t> LanternStruct<'t> {
    pub fn new(name: Symbol, index: usize) -> Self {
        Self {
            name,
            id: index,
            data: OnceCell::new(),
        }
    }

    pub fn init(&self, fields: Box<[(Symbol, TypeId<'t>)]>) {
        if self.data.set(LanternStructData::new(fields)).is_err() {
            panic!("double-init on lantern struct")
        }
    }

    pub fn data(&self) -> &LanternStructData<'t> {
        match self.data.get() {
            Some(data) => data,
            None => panic!("struct data not initialized"),
        }
    }

    pub fn find_field(&self, name: Symbol) -> Option<&LanternStructField<'t>> {
        self.data().find_field(name)
    }

    pub fn to_type_info(&self) -> TypeInfo {
        self.data().to_type_info()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStructData<'t> {
    fields: Box<[LanternStructField<'t>]>,
    size: usize,
}

impl<'t> LanternStructData<'t> {
    pub fn new(fields: Box<[(Symbol, TypeId<'t>)]>) -> Self {
        let alignment = fields.iter()
            .map(|(_, ty)| ty.alignment())
            .max()
            .unwrap_or(1);

        let mut size = 0;
        let fields = fields.into_iter()
            .map(|(name, ty)| {
                size += size % ty.alignment();
                let field = LanternStructField { name, offset: size, ty };
                size += ty.size();
                field
            })
            .collect();
        size += size % alignment;

        Self {
            fields,
            size,
        }
    }

    pub fn find_field(&self, name: Symbol) -> Option<&LanternStructField<'t>> {
        self.fields.iter().find(|field| field.name == name)
    }

    pub fn to_type_info(&self) -> TypeInfo {
        TypeInfo::Object {
            size: self.size,
            ref_offets: self.fields.iter()
                .filter(|field| field.ty.is_ref())
                .map(|field| field.offset)
                .collect(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LanternStructField<'t> {
    pub name: Symbol,
    pub offset: usize,
    pub ty: TypeId<'t>,
}

#[derive(Clone)]
pub struct LanternPrimitive {
    pub name: &'static str,
    pub id: usize,
    pub size: usize,
    pub align: usize,
    pub ops: PrimitiveOps,
}

impl hash::Hash for LanternPrimitive {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.id);
    }
}

impl std::fmt::Debug for LanternPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LanternPrimitive")
            .field("name", &self.name)
            .field("id", &self.id)
            .field("size", &self.size)
            .field("align", &self.align)
            .finish_non_exhaustive()
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct PrimitiveOps {
    pub negate_inst: Option<Instruction>,
    pub not_inst: Option<Instruction>,
    pub add_inst: Option<Instruction>,
    pub sub_inst: Option<Instruction>,
    pub mult_inst: Option<Instruction>,
    pub div_inst: Option<Instruction>,
    pub mod_inst: Option<Instruction>,
    pub lt_inst: Option<Instruction>,
    pub le_inst: Option<Instruction>,
    pub ge_inst: Option<Instruction>,
    pub gt_inst: Option<Instruction>,
    pub eq_inst: Option<Instruction>,
}

impl PrimitiveOps {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_bin_op(&self, op: &BinaryOperator) -> Option<Instruction> {
        match op {
            BinaryOperator::Add(_) | BinaryOperator::AddAssign(_) => self.add_inst.clone(),
            BinaryOperator::Sub(_) | BinaryOperator::SubAssign(_) => self.sub_inst.clone(),
            BinaryOperator::Mult(_) | BinaryOperator::MultAssign(_) => self.mult_inst.clone(),
            BinaryOperator::Div(_) | BinaryOperator::DivAssign(_) => self.div_inst.clone(),
            BinaryOperator::Mod(_) | BinaryOperator::ModAssign(_) => self.mod_inst.clone(),
            BinaryOperator::Lt(_) => self.lt_inst.clone(),
            BinaryOperator::Le(_) => self.le_inst.clone(),
            BinaryOperator::Gt(_) => self.gt_inst.clone(),
            BinaryOperator::Ge(_) => self.ge_inst.clone(),
            BinaryOperator::Eq(_) | BinaryOperator::Neq(_) => self.eq_inst.clone(),
            _ => None,
        }
    }

    pub fn get_un_op(&self, op: &UnaryOperator) -> Option<Instruction> {
        match op {
            UnaryOperator::Not(_) => self.not_inst.clone(),
            UnaryOperator::Negate(_) => self.negate_inst.clone(),
        }
    }
}

impl PartialEq for LanternPrimitive {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LanternPrimitive { }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LanternVariable<'t> {
    pub index: usize,
    pub ty: TypeId<'t>,
}

impl<'t> LanternVariable<'t> {
    pub fn new(index: usize, ty: TypeId<'t>) -> Self {
        Self { index, ty }
    }
}

#[derive(Debug, Clone)]
pub struct GeneratedFunction {
    pub line_table: Vec<LineMap>,
    pub name: Box<str>,
    pub kind: FunctionKind,
}

impl GeneratedFunction {
    pub fn new(name: Box<str>, kind: FunctionKind) -> Self {
        Self { line_table: Vec::new(), name, kind }
    }

    pub fn line_for(&self, inst_ptr: usize) -> u32 {
        match self.line_table.binary_search_by_key(&inst_ptr, |map| map.ip) {
            Ok(i) => self.line_table[i].line,
            Err(i) => self.line_table[i - 1].line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum FunctionKind {
    Instructions(InstructionSet, usize),
    Native(NativeFn),
}

