use std::{fmt::{Display, Formatter}, ops::ControlFlow};

use diagnostic::{DiagnosticSink, error};
use instruction::InstructionSet;
use parse::{FunArg, IfBranch, IfStmt, Item, ItemFun, ItemNativeFun, ItemPrimitive, ItemStruct, LanternFile, ReturnStmt, Stmt, StructField, ValDeclaration, WhileStmt, expr::{BinaryOperator, Expr, ExprArray, ExprBinary, ExprBlock, ExprField, ExprFunCall, ExprIndex, ExprParen, ExprStruct, ExprUnary, UnaryOperator}, lex::{Break, Ident, Literal, TokenKind}};

use crate::{Slot, VM, error::RuntimeError, flame::{instruction::Instruction, scope::{Globals, ItemIdentifier, LoopContext, LoopScope, Scope, ScopeKind, StackFrame}, r#type::LanternType}, heap::{HeapArray, HeapObject, ObjectHeader, TypeInfo}, inst};

pub type NativeFn = fn(&mut VM) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod r#type;
pub mod scope;
pub mod native;

pub fn ignite(file: LanternFile, globals: &mut Globals, sink: &mut DiagnosticSink) -> GeneratedFunction {
    let mut r#gen = FlameGen::new(globals, sink);
    let _ = r#gen.compile_stmts(file.stmts, Scope::new());
    r#gen.frame.into_gen()
}

#[derive(Debug)]
pub struct FlameGen<'a> {
    pub frame: StackFrame,
    pub globals: &'a mut Globals,
    pub sink: &'a mut DiagnosticSink,
    loop_context: LoopContext,
}

impl<'a> FlameGen<'a> {
    pub fn new(globals: &'a mut Globals, sink: &'a mut DiagnosticSink) -> Self {
        Self {
            frame: StackFrame::new_module(),
            globals,
            sink,
            loop_context: LoopContext::new(),
        }
    }

    pub fn using_frame<F: FnOnce(&mut Self)>(&mut self, mut frame: StackFrame, fun: F) -> GeneratedFunction {
        std::mem::swap(&mut self.frame, &mut frame);
        fun(self);
        std::mem::swap(&mut self.frame, &mut frame);
        frame.into_gen()
    }

    pub fn compile_stmts(&mut self, statements: Vec<Stmt>, mut scope: Scope) -> ControlFlow<()> {
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
                Item::Using(_) => todo!(),
                Item::Fun(ItemFun { path, args, ret, .. }) => {
                    let args = args.iter()
                        .map(|FunArg { ident, r#type, .. }| (ident.clone(), self.sink.emit_or(LanternType::from_type(r#type, &scope), LanternType::Null)))
                        .collect();

                    let ret = ret.as_ref()
                        .map(|(_, r#type)| self.sink.emit_or(LanternType::from_type(r#type, &scope), LanternType::Null))
                        .unwrap_or(LanternType::Null);

                    let name = path.last().0.clone();
                    let fun = LanternFunction::new(self.globals.funs.len(), args, ret);
                    if path.items.len() == 1 {
                        if scope.insert_function(name, fun).is_none() {
                            error!(in self.sink; path.last().span() => "function already declared");
                        }
                    } else {
                        let ident = &path.items[0];
                        if let Some(item) = scope.item(&ident.0) {
                            if scope.insert_associated(item.identifier(), name, fun).is_none() {
                                error!(in self.sink; ident.span() => "associated function already declared");
                            }
                        } else {
                            error!(in self.sink; ident.span() => "item {ident} not found");
                        }
                    }
                    self.globals.funs.push(GeneratedFunction::Native(native::dummy_native));
                },
                Item::NativeFun(ItemNativeFun { ident, args, ret, .. }) => {
                    let args = args.iter()
                        .map(|FunArg { ident, r#type, .. }| (ident.clone(), self.sink.emit_or(LanternType::from_type(r#type, &scope), LanternType::Null)))
                        .collect();

                    let ret = ret.as_ref()
                        .map(|(_, r#type)| self.sink.emit_or(LanternType::from_type(r#type, &scope), LanternType::Null))
                        .unwrap_or(LanternType::Null);

                    scope.insert_function(ident.0.clone(), LanternFunction::new(self.globals.funs.len(), args, ret));

                    let ptr = native::get_native_fn(&ident.0).unwrap_or_else(|| {
                        error!(in self.sink; ident.span() => "unknown native `{}`", ident.0);
                        native::dummy_native
                    });

                    self.globals.funs.push(GeneratedFunction::Native(ptr));
                },
                // TODO: add types before checking for types
                Item::Struct(ItemStruct { ident, fields, .. }) => {
                    let fields = fields.iter()
                        .map(|StructField { ident, r#type, .. }| {
                            (ident.0.clone(), self.sink.emit_or(LanternType::from_type(r#type, &scope), LanternType::Null))
                        })
                        .collect();

                    let lantern_struct = LanternStruct::new(self.globals.types.len(), fields);
                    self.globals.types.push(lantern_struct.to_type_info());
                    scope.insert_item(ident.0.clone(), LanternItem::Struct(lantern_struct));
                },
                Item::Primitive(ItemPrimitive { ident, .. }) => {
                    let Some(primitive) = native::get_primitive(&ident.0) else { panic!("unknown primitive `{}`", ident.0) };
                    scope.insert_item(ident.0.clone(), LanternItem::Primitive(primitive));
                },
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
                                let r#type = self.compile_expr(condition, &scope)?;
                                if !r#type.is_bool() {
                                    error!(in self.sink; condition_span => "expected `bool`, but got {type} instead");
                                }

                                let false_index = self.frame.instructions.len();
                                inst!(self.frame.instructions; GOTO_IF_FALSE 0);

                                let block_scope = scope.child_block();
                                let branch_return = self.compile_stmts(block.stmts, block_scope);
                                match (overall_return, branch_return) {
                                    (Some(ControlFlow::Break(_)), ControlFlow::Break(_)) => {},
                                    (Some(ControlFlow::Break(_)), ControlFlow::Continue(_)) => overall_return = Some(branch_return),
                                    (Some(ControlFlow::Continue(_)), _) => {},
                                    (None, _) => overall_return = Some(branch_return),
                                }
                                end_indices.push(self.frame.instructions.len());
                                inst!(self.frame.instructions; GOTO 0);
                                self.frame.instructions[false_index] = Instruction::PopGotoIfFalse(self.frame.instructions.len());

                                match branch {
                                    Some((_, next_branch)) => current_branch = *next_branch,
                                    None => break,
                                }
                            },
                            IfBranch::Else(block) => {
                                let block_scope = scope.child_block();
                                let branch_return = self.compile_stmts(block.stmts, block_scope);
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

                    let r#type = self.compile_expr(condition, &scope)?;
                    if !r#type.is_bool() {
                        error!(in self.sink; condition_span => "expected `bool`, but got {type} instead");
                    }
                    let condition_index = self.frame.instructions.len();
                    inst!(self.frame.instructions; POP_GOTO_IF_FALSE 0);

                    self.loop_context.scopes.push(LoopScope::new(head));
                    let block_scope = scope.child_block();
                    // we can't assume the initial condition is met so these may not even be ran
                    let _ = self.compile_stmts(block.stmts, block_scope);
                    inst!(self.frame.instructions; GOTO head);

                    self.frame.instructions[condition_index] = Instruction::PopGotoIfFalse(self.frame.instructions.len());

                    for break_index in self.loop_context.scopes.pop().expect("in loop").breaks {
                        self.frame.instructions[break_index] = Instruction::Goto(self.frame.instructions.len());
                    }
                },
                Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: None, .. }) => {
                    // TODO: unitialized vars
                    let local_index = self.frame.declare_local(ident.0.clone());
                    if scope.insert_variable(ident.0.clone(), self.sink.emit_or(LanternType::from_type(&r#type, &scope), LanternType::Null)).is_none() {
                        error!(in self.sink; ident.span() => "variable `{}` already declared", ident.0);
                    }
                    inst! { self.frame.instructions;
                        [PUSHU 0]
                        [STORE_LOCAL local_index]
                        [POP]
                    }
                },
                Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                    let init_span = init.span();
                    let init_type = self.compile_expr(init, &scope)?;

                    let var_type = self.sink.emit_or(LanternType::from_type(&r#type, &scope), LanternType::Null);
                    if var_type != init_type {
                        error!(in self.sink; init_span => "expected {var_type}, but got {init_type} instead");
                    }
                    let local_index = self.frame.declare_local(ident.0.clone());
                    if scope.insert_variable(ident.0.clone(), var_type).is_none() {
                        error!(in self.sink; ident.span() => "variable `{}` already declared", ident.0);
                    }
                    inst! { self.frame.instructions;
                        [STORE_LOCAL local_index]
                        [POP]
                    };
                },
                Stmt::Return(ReturnStmt { ret: ret_keyword, expr, .. }) => {
                    let expected_ret = match &self.frame.ret_type {
                        Some(ret) => ret.clone(),
                        _ => {
                            error!(in self.sink; ret_keyword.span() => "{ret_keyword} not allowed here");
                            return ControlFlow::Break(());
                        },
                    };
                    let ret = if let Some(expr) = expr {
                        self.compile_expr(expr, &scope)?
                    } else {
                        inst!(self.frame.instructions; PUSHU 0);
                        LanternType::Null
                    };
                    if expected_ret != ret {
                        error!(in self.sink; ret_keyword.span() => "expected {expected_ret}, but got {ret} instead");
                    }
                    inst!(self.frame.instructions; RET);
                    return ControlFlow::Break(());
                },
                Stmt::Continue(continue_keyword, _) => {
                    if let Some(LoopScope { head, .. }) = self.loop_context.scopes.last() {
                        inst!(self.frame.instructions; GOTO *head);
                    } else {
                        error!(in self.sink; continue_keyword.span() => "{continue_keyword} not allowed here");
                    }
                },
                Stmt::Break(Break(span), _) => {
                    if let Some(LoopScope { breaks, .. }) = self.loop_context.scopes.last_mut() {
                        breaks.push(self.frame.instructions.len());
                        inst!(self.frame.instructions; GOTO 0);
                    } else {
                        error!(in self.sink; span => "`break` not allowed here");
                    }
                },
                Stmt::Throw(_, expr, _) => {
                    let span = expr.span();
                    let ty = self.compile_expr(expr, &scope)?;
                    // TODO: string type
                    if ty != LanternType::Array(Box::new(LanternType::Primitive(&native::BYTE_PRIMITIVE))) {
                        error!(in self.sink; span => "expected `[u8]`, but got {ty} instead");
                    }
                    inst!(self.frame.instructions; THRW);
                },
                Stmt::Expr(expr, _) => {
                    self.compile_expr(expr, &scope)?;
                    inst!(self.frame.instructions; POP);
                },
                Stmt::Item(Item::Using(_)) => todo!(),
                Stmt::Item(Item::Fun(ItemFun { path, block, ret, .. })) => {
                    let ret = ret
                        .map(|(_, r#type)| self.sink.emit_or(LanternType::from_type(&r#type, &scope), LanternType::Null))
                        .unwrap_or(LanternType::Null);

                    let fun = if path.items.len() == 1 {
                        scope.function(&path.last().0).expect("function in scope")
                    } else {
                        scope.associated(scope.item(&path.items[0].0).expect("item in scope").identifier(), &path.last().0).expect("assosiated in scope")
                    };
                    let name = path.into_last().0;

                    let mut fun_scope = scope.child_function(block.open_brace.span());
                    let mut fun_frame = StackFrame::new_fun(name, ret);

                    for (ident, r#type) in &fun.args {
                        fun_frame.declare_local(ident.0.clone());
                        if fun_scope.insert_variable(ident.0.clone(), r#type.clone()).is_none() {
                            error!(in self.sink; ident.span() => "argument `{}` already declared", ident.0);
                        }
                    }

                    let generated = self.using_frame(fun_frame, |nested| {
                        let _ = nested.compile_stmts(block.stmts, fun_scope);
                    });

                    self.globals.funs[fun.index] = generated;
                },
                Stmt::Item(Item::NativeFun(_)) => {},
                Stmt::Item(Item::Struct(_)) => {},
                Stmt::Item(Item::Primitive(_)) => {},
            }
        };

        match scope.into_kind() {
            // implicit return
            ScopeKind::Function(_, span) => {
                let ret_type = self.frame.ret_type.clone().expect("function scope has return type");
                if ret_type != LanternType::Null {
                    error!(in self.sink; span => "expected function to return {}", LanternType::Null);
                };
                inst! { self.frame.instructions;
                    [PUSHU 0]
                    [RET]
                }
                ControlFlow::Break(())
            },
            ScopeKind::Module => {
                inst! { self.frame.instructions;
                    [PUSHU 0]
                    [RET]
                }
                ControlFlow::Break(())
            },
            ScopeKind::Block(_) => ControlFlow::Continue(())
        }
    }

    pub fn compile_expr(&mut self, expression: Expr, scope: &Scope) -> ControlFlow<(), LanternType> {
        match expression {
            Expr::Literal(Literal::Integer(int, _)) => {
                inst!(self.frame.instructions; PUSHI int);
                ControlFlow::Continue(LanternType::Primitive(&native::INT_PRIMITIVE))
            },
            Expr::Literal(Literal::Float(float, _)) => {
                inst!(self.frame.instructions; PUSHF float);
                ControlFlow::Continue(LanternType::Primitive(&native::FLOAT_PRIMITIVE))
            },
            Expr::Literal(Literal::True(_)) => {
                inst!(self.frame.instructions; PUSHU 1);
                ControlFlow::Continue(LanternType::Primitive(&native::BOOL_PRIMITIVE))
            },
            Expr::Literal(Literal::False(_)) => {
                inst!(self.frame.instructions; PUSHU 0);
                ControlFlow::Continue(LanternType::Primitive(&native::BOOL_PRIMITIVE))
            },
            Expr::Literal(Literal::String(string, _)) => {
                // TODO: better string alloc
                inst!(self.frame.instructions; ALLOC_STR string.clone());
                // TODO: make string a struct instead of array
                ControlFlow::Continue(LanternType::Array(Box::new(LanternType::Primitive(&native::BYTE_PRIMITIVE))))
            },
            Expr::FunCall(ExprFunCall { expr, args, .. }) => {
                let span = expr.span();
                let r#type = self.compile_expr(*expr, scope)?;
                if let LanternType::Function { is_method, args: fun_args, ret } = r#type {
                    let fun_args_len = fun_args.len();
                    if args.len() != fun_args_len {
                        error!(in self.sink; span => "expected function to have {} args, got {} args instead", fun_args_len, args.len());
                    }

                    for (expr, r#type) in args.into_iter().zip(fun_args) {
                        let expr_span = expr.span();
                        let expr_type = self.compile_expr(expr, scope)?;
                        if expr_type != r#type {
                            error!(in self.sink; expr_span => "expected {type}, got {expr_type} instead");
                        }
                    }

                    if is_method {
                        inst!(self.frame.instructions; INV_MET fun_args_len);
                    } else {
                        inst!(self.frame.instructions; INV fun_args_len);
                    }

                    ControlFlow::Continue(*ret)
                } else {
                    error!(in self.sink; span => "expected function");
                    ControlFlow::Continue(LanternType::Null)
                }
            },
            Expr::Binary(ExprBinary { lhs, op, rhs }) => {
                // special cases
                match op {
                    BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                        let lhs_type = self.compile_expr(*lhs, scope)?;
                        let goto_index = self.frame.instructions.len();

                        match op {
                            BinaryOperator::And(_) => inst!(self.frame.instructions; GOTO_IF_FALSE 0),
                            BinaryOperator::Or(_) => inst!(self.frame.instructions; GOTO_IF_TRUE 0),
                            _ => unreachable!(),
                        };
                        inst!(self.frame.instructions; POP);

                        let rhs_type = self.compile_expr(*rhs, scope)?;

                        let goto_inst = match op {
                            BinaryOperator::And(_) => Instruction::GotoIfFalse(self.frame.instructions.len()),
                            BinaryOperator::Or(_) => Instruction::GotoIfTrue(self.frame.instructions.len()),
                            _ => unreachable!(),
                        };
                        self.frame.instructions[goto_index] = goto_inst;

                        if !lhs_type.is_bool() || !rhs_type.is_bool() {
                            error!(in self.sink; op.span() => "{op} cannot be applied to {lhs_type} and {rhs_type}");
                        }

                        return ControlFlow::Continue(LanternType::Primitive(&native::BOOL_PRIMITIVE));
                    },
                    BinaryOperator::Assign(punct) => {
                        let rhs_span = rhs.span();
                        match *lhs {
                            Expr::Identifier(ident) => {
                                let Some(var) = scope.variable(&ident.0) else {
                                    error!(in self.sink; ident.span() => "unknown variable `{}`", ident.0);
                                    return ControlFlow::Continue(LanternType::Null);
                                };

                                let rhs = self.compile_expr(*rhs, scope)?;

                                if var.r#type != rhs {
                                    error!(in self.sink; rhs_span => "expected {}, but got {rhs} instead", var.r#type);
                                }
                                let local_index = self.frame.find_local(&ident.0).expect("local var exists");
                                inst!(self.frame.instructions; STORE_LOCAL local_index);
                            },
                            Expr::Index(ExprIndex { expr, index, .. }) => {
                                let expr_span = expr.span();
                                let r#type = self.compile_expr(*expr, scope)?;
                                let inner = match r#type {
                                    LanternType::Array(inner) => *inner,
                                    _ => {
                                        error!(in self.sink; expr_span => "expected array or string");
                                        LanternType::Null
                                    },
                                };

                                let index_span = index.span();
                                let index_type = self.compile_expr(*index, scope)?;
                                if index_type != LanternType::Primitive(&native::INT_PRIMITIVE) {
                                    error!(in self.sink; index_span => "expected index to be an `int`");
                                }

                                inst! { self.frame.instructions;
                                    [PUSHU inner.size() as u64]
                                    [MULTI]
                                    [PUSHU HeapArray::element_offset() as u64]
                                    [ADDI]
                                }

                                let rhs = self.compile_expr(*rhs, scope)?;

                                if rhs != inner {
                                    error!(in self.sink; rhs_span => "expected {inner}, but got {rhs} instead");
                                }

                                // TODO: bounds checking
                                inst!(self.frame.instructions; WRITE inner.size());
                            },
                            Expr::Field(ExprField { expr, ident }) => {
                                let expr_span = expr.span();
                                let ty = self.compile_expr(*expr, scope)?;
                                match ty {
                                    LanternType::Struct(type_id) => {
                                        if let Some(field) = scope.find_struct(type_id).fields.iter().find(|field| field.name == ident.0) {
                                            inst!(self.frame.instructions; PUSHU (HeapObject::field_offset() + field.offset) as u64);
                                            let field_type = self.compile_expr(*rhs, scope)?;
                                            if field_type != field.r#type {
                                                error!(in self.sink; rhs_span => "expected {}, but got {field_type} instead", field.r#type);
                                            }
                                            inst! (self.frame.instructions; WRITE field.size);
                                        } else {
                                            // TODO: type name
                                            error!(in self.sink; expr_span => "field `{}` does not exist", ident.0);
                                        }
                                    },
                                    _ => error!(in self.sink; expr_span => "field `{}` does not exist on {ty}", ident.0),
                                }
                            },
                            _ => error!(in self.sink; punct.span() => "bad left-hand-side of assignment"),
                        }
                        return ControlFlow::Continue(LanternType::Null);
                    },
                    _ => {},
                }

                let lhs = self.compile_expr(*lhs, scope)?;
                let rhs = self.compile_expr(*rhs, scope)?;

                match (lhs, op, rhs) {
                    (LanternType::Primitive(lhs), op, LanternType::Primitive(rhs)) if lhs == rhs && op.is_comparison() && lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                        ControlFlow::Continue(LanternType::Primitive(&native::BOOL_PRIMITIVE))
                    },
                    (LanternType::Primitive(lhs), op, LanternType::Primitive(rhs)) if lhs == rhs && lhs.ops.get_bin_op(&op).is_some() => {
                        self.frame.instructions.push(lhs.ops.get_bin_op(&op).unwrap());
                        ControlFlow::Continue(LanternType::Primitive(lhs))
                    },
                    (_, BinaryOperator::Assign(_) | BinaryOperator::And(_) | BinaryOperator::Or(_), _) => unreachable!(),
                    (lhs, op, rhs) => {
                        error!(in self.sink; op.span() => "{op} cannot be applied to {lhs} and {rhs}");
                        ControlFlow::Continue(LanternType::Null)
                    },
                }
            },
            Expr::Unary(ExprUnary { op, expr }) => {
                let r#type = self.compile_expr(*expr, scope)?;
                match (op, r#type) {
                    (op, LanternType::Primitive(primitive)) if primitive.ops.get_un_op(&op).is_some() => {
                        self.frame.instructions.push(primitive.ops.get_un_op(&op).unwrap());
                        ControlFlow::Continue(LanternType::Primitive(primitive))
                    },
                    (op, got) => {
                        error!(in self.sink; op.span() => "{op} cannot be applied to {got}");
                        ControlFlow::Continue(LanternType::Null)
                    },
                }
            },
            Expr::Struct(ExprStruct { ident, mut fields, .. }) => {
                let Some(LanternItem::Struct(r#struct)) = scope.item(&ident.0) else {
                    error!(in self.sink; ident.span() => "unknown struct");
                    return ControlFlow::Continue(LanternType::Null);
                };
                inst!(self.frame.instructions; ALLOC_OBJ r#struct.id);
                for field in &r#struct.fields {
                    match fields.iter().position(|expr_field| expr_field.ident.0 == field.name) {
                        Some(index) => {
                            let expr_field = fields.swap_remove(index);
                            let expr_span = expr_field.expr.span();
                            inst!(self.frame.instructions; PUSHU (HeapObject::field_offset() + field.offset) as u64);
                            let field_ty = self.compile_expr(expr_field.expr, scope)?;
                            if field_ty != field.r#type {
                                error!(in self.sink; expr_span => "expected {}, but got {field_ty} instead", field.r#type);
                            }
                            inst!(self.frame.instructions; WRITE field.size);
                        },
                        None => error!(in self.sink; ident.span() => "missing field `{}`", field.name),
                    }
                }

                for extraneous_field in fields {
                    error!(in self.sink; extraneous_field.ident.span() => "unknown field");
                }

                ControlFlow::Continue(LanternType::Struct(r#struct.id))
            },
            Expr::Paren(ExprParen { expr, .. }) => self.compile_expr(*expr, scope),
            Expr::Block(ExprBlock { stmts, .. }) => {
                let block_scope = scope.child_block();
                self.compile_stmts(stmts, block_scope)?;
                inst!(self.frame.instructions; PUSHU 0);
                ControlFlow::Continue(LanternType::Null)
            },
            Expr::Array(ExprArray { elements, .. }) => {
                let len = elements.len();
                let mut inner = None;

                for expr in elements {
                    let span = expr.span();
                    inner = match (inner, self.compile_expr(expr, scope)?) {
                        (None, r#type) => Some(r#type),
                        (Some(r#type), expr_type) if r#type == expr_type => Some(r#type),
                        (Some(r#type), expr_type) => {
                            error!(in self.sink; span => "expected {type}, but got {expr_type} instead");
                            Some(r#type)
                        },
                    }
                }

                inst!(self.frame.instructions; ALLOC_ARR VM::PRIMITIVE_ARR_TYPE_INDEX, len);
                match inner {
                    Some(inner) => ControlFlow::Continue(LanternType::Array(Box::new(inner))),
                    // TODO: type hint
                    None => ControlFlow::Continue(LanternType::Array(Box::new(LanternType::Null))),
                }
            },
            Expr::Index(ExprIndex { expr, index, .. }) => {
                let expr_span = expr.span();
                let r#type = self.compile_expr(*expr, scope)?;
                let inner = match r#type {
                    LanternType::Array(inner) => *inner,
                    _ => {
                        error!(in self.sink; expr_span => "expected array or string");
                        LanternType::Null
                    },
                };
                let index_span = index.span();
                let index_type = self.compile_expr(*index, scope)?;
                if index_type != LanternType::Primitive(&native::INT_PRIMITIVE) {
                    error!(in self.sink; index_span => "expected index to be an `int`");
                }

                inst! { self.frame.instructions;
                    [PUSHU inner.size() as u64]
                    [MULTI]
                    [PUSHU HeapArray::element_offset() as u64]
                    [ADDI]
                    [ADDI]
                    [READ if inner.is_primitive() { inner.size() } else { 0 }]
                }
                ControlFlow::Continue(inner)
            },
            Expr::Identifier(ident) => {
                let span = ident.span();
                if let Some(var) = scope.variable(&ident.0) {
                    let local_index = self.frame.find_local(&ident.0).expect("local var exists");
                    inst!(self.frame.instructions; LOAD_LOCAL local_index);
                    ControlFlow::Continue(var.r#type)
                } else if let Some(fun) = scope.function(&ident.0) {
                    inst!(self.frame.instructions; PUSHU fun.index as u64);
                    ControlFlow::Continue(fun.to_assoc_type())
                } else if let Some(item) = scope.item(&ident.0) {
                    ControlFlow::Continue(LanternType::ItemStatic(item.identifier()))
                } else {
                    error!(in self.sink; span => "unknown identifier `{}`", ident.0);
                    ControlFlow::Continue(LanternType::Null)
                }
            },
            Expr::Field(ExprField { expr, ident }) => {
                let ty = self.compile_expr(*expr, scope)?;
                match ty {
                    LanternType::Struct(type_id) => {
                        if let Some(field) = scope.find_struct(type_id).fields.iter().find(|field| field.name == ident.0) {
                            let size = if field.r#type.is_primitive() { field.size } else { 0 };
                            inst! { self.frame.instructions;
                                [PUSHU (HeapObject::field_offset() + field.offset) as u64]
                                [ADDI]
                                [READ size]
                            }
                            ControlFlow::Continue(field.r#type.clone())
                        } else if let Some(associated) = scope.associated(ItemIdentifier::Struct(type_id), &ident.0) {
                            if associated.args.first().is_some_and(|(_, receiver)| *receiver == ty) {
                                inst!(self.frame.instructions; PUSHU associated.index as u64);
                            } else {
                                error!(in self.sink; ident.1 => "method must have a receiver");
                            }
                            ControlFlow::Continue(associated.to_method_type())
                        } else {
                            // TODO: type name
                            error!(in self.sink; ident.1 => "field {} does not exist", ident.0);
                            ControlFlow::Continue(LanternType::Null)
                        }
                    },
                    LanternType::Array(inner) => {
                        if ident.0 == "len" {
                            let size = if inner.is_primitive() { inner.size() } else { 0 };
                            inst! { self.frame.instructions;
                                [PUSHU size_of::<ObjectHeader>() as u64]
                                    [ADDI]
                                        [READ size]
                            }
                            ControlFlow::Continue(LanternType::Primitive(&native::INT_PRIMITIVE))
                        } else {
                            error!(in self.sink; ident.1 => "field {} does not exist on array", ident.0);
                            ControlFlow::Continue(LanternType::Null)
                        }
                    },
                    LanternType::Primitive(primitive) => {
                        if let Some(associated) = scope.associated(ItemIdentifier::Primitive(primitive.id), &ident.0) {
                            if associated.args.first().is_some_and(|(_, receiver)| *receiver == ty) {
                                inst!(self.frame.instructions; PUSHU associated.index as u64);
                            } else {
                                error!(in self.sink; ident.1 => "method must have a receiver");
                            }
                            ControlFlow::Continue(associated.to_method_type())
                        } else {
                            // TODO: type name
                            error!(in self.sink; ident.1 => "method {} does not exist", ident.0);
                            ControlFlow::Continue(LanternType::Null)
                        }
                    },
                    LanternType::ItemStatic(type_id) => {
                        let Some(fun) = scope.associated(type_id, &ident.0) else {
                            error!(in self.sink; ident.span() => "static item {} does not exist", ident.0);
                            return ControlFlow::Continue(LanternType::Null)
                        };
                        inst!(self.frame.instructions; PUSHU fun.index as u64);
                        ControlFlow::Continue(fun.to_assoc_type())
                    },
                    _ => {
                        error!(in self.sink; ident.1 => "field {} does not exist on {ty}", ident.0);
                        ControlFlow::Continue(LanternType::Null)
                    },
                }
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternFunction {
    pub index: usize,
    pub args: Vec<(Ident, LanternType)>,
    pub ret: LanternType,
}

impl LanternFunction {
    pub fn new(index: usize, args: Vec<(Ident, LanternType)>, ret: LanternType) -> Self {
        Self { index, args, ret }
    }

    pub fn to_assoc_type(&self) -> LanternType {
        LanternType::Function { is_method: false, args: self.args.iter().map(|(_, r#type)| r#type.clone()).collect(), ret: Box::new(self.ret.clone()) }
    }

    pub fn to_method_type(&self) -> LanternType {
        // first arg is assumed to be the receiver
        LanternType::Function { is_method: true, args: self.args.iter().skip(1).map(|(_, r#type)| r#type.clone()).collect(), ret: Box::new(self.ret.clone()) }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LanternItem {
    Struct(LanternStruct),
    Primitive(&'static LanternPrimitive),
}

impl LanternItem {
    pub fn identifier(&self) -> ItemIdentifier {
        match self {
            Self::Struct(r#struct) => ItemIdentifier::Struct(r#struct.id),
            Self::Primitive(primitive) => ItemIdentifier::Primitive(primitive.id),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStruct {
    pub id: usize,
    pub fields: Box<[LanternStructField]>,
    pub size: usize,
}

impl LanternStruct {
    pub fn new(index: usize, fields: Box<[(String, LanternType)]>) -> Self {
        let alignment = fields.iter()
            .map(|(_, r#type)| r#type.alignment())
            .max()
            .unwrap_or(1);

        let mut struct_fields = Vec::with_capacity(fields.len());
        let mut size = 0;
        for (name, r#type) in fields {
            let padding = size % r#type.alignment();
            size += padding + r#type.size();
            struct_fields.push(LanternStructField { name, offset: size + padding, size: r#type.size(), r#type });
        }
        size += size % alignment;

        Self {
            id: index,
            fields: struct_fields.into(),
            size,
        }
    }

    pub fn to_type_info(&self) -> TypeInfo {
        TypeInfo::Object {
            size: self.size,
            ref_offets: self.fields.iter()
                .filter(|field| field.r#type.is_ref())
                .map(|field| field.offset)
                .collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct LanternPrimitive {
    pub id: usize,
    pub size: usize,
    pub align: usize,
    pub ops: PrimitiveOps,
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
            BinaryOperator::Add(_) => self.add_inst.clone(),
            BinaryOperator::Sub(_) => self.sub_inst.clone(),
            BinaryOperator::Mult(_) => self.mult_inst.clone(),
            BinaryOperator::Div(_) => self.div_inst.clone(),
            BinaryOperator::Mod(_) => self.mod_inst.clone(),
            BinaryOperator::Lt(_) => self.lt_inst.clone(),
            BinaryOperator::Le(_) => self.le_inst.clone(),
            BinaryOperator::Gt(_) => self.gt_inst.clone(),
            BinaryOperator::Ge(_) => self.ge_inst.clone(),
            BinaryOperator::Eq(_) => self.eq_inst.clone(),
            _ => None,
        }
    }

    pub fn get_un_op(&self, op: &UnaryOperator) -> Option<Instruction> {
        match op {
            UnaryOperator::Negate(_) => self.negate_inst.clone(),
            _ => None,
        }
    }
}

impl PartialEq for LanternPrimitive {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LanternPrimitive { }

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStructField {
    pub name: String,
    pub offset: usize,
    pub size: usize,
    pub r#type: LanternType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternVariable {
    pub r#type: LanternType,
}

impl LanternVariable {
    pub fn new(r#type: LanternType) -> Self {
        Self { r#type }
    }
}

#[derive(Debug, Clone)]
pub enum GeneratedFunction {
    Instructions(InstructionSet, usize),
    Native(NativeFn),
}

impl Display for GeneratedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Instructions(instructions, locals) => write!(f, "{locals} local(s)\n{instructions}"),
            Self::Native(ptr) => write!(f, "<native function @ {ptr:?}>"),
        }
    }
}

