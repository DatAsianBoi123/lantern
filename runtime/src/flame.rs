use std::{fmt::{Display, Formatter}, mem::MaybeUninit, ops::ControlFlow, rc::Rc};

use diagnostic::{DiagnosticSink, error};
use instruction::InstructionSet;
use parse::{FunArg, IfBranch, IfStmt, Item, ItemFun, ItemNative, LanternFile, Stmt, ValDeclaration, WhileStmt, expr::{BinaryOperator, Expr, ExprArray, ExprBinary, ExprBlock, ExprFunCall, ExprIndex, ExprParen, ExprUnary, UnaryOperator}, lex::{Break, Ident, Literal, TokenKind}};

use crate::{Slot, VM, error::RuntimeError, flame::{instruction::Instruction, scope::{Globals, Scope, ScopeKind, StackFrame}, r#type::LanternType}, inst};

pub type GenerateFunPtr = Rc<MaybeUninit<GeneratedFunction>>;
pub type NativeFn = fn(&mut VM, [Slot; 256]) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod r#type;
pub mod scope;
pub mod native;

pub fn ignite(file: LanternFile, globals: &mut Globals, sink: &mut DiagnosticSink) -> GeneratedFunction {
    let mut frame = StackFrame::new_module();
    let _ = compile_stmts(file.stmts, Scope::new(), &mut frame, globals, sink);
    frame.into_gen()
}

fn compile_stmts(statements: Vec<Stmt>, mut scope: Scope, frame: &mut StackFrame, globals: &mut Globals, sink: &mut DiagnosticSink) -> ControlFlow<()> {
    let mut next_fun_index = globals.funs.len();
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
                Item::Fun(ItemFun { ident, args, ret, .. }) => {
                    let args = args.0.iter()
                        .map(|FunArg { ident, r#type, .. }| (ident.clone(), sink.emit_or(LanternType::from_type(r#type), LanternType::Null)))
                        .collect();

                    let ret = ret.as_ref()
                        .map(|(_, r#type)| sink.emit_or(LanternType::from_type(r#type), LanternType::Null))
                        .unwrap_or(LanternType::Null);

                    scope.insert_function(ident.0.clone(), LanternFunction::new(next_fun_index, args, ret));
                    next_fun_index += 1;
                },
                Item::Native(ItemNative { ident, args, ret, .. }) => {
                    let args = args.0.iter()
                        .map(|FunArg { ident, r#type, .. }| (ident.clone(), sink.emit_or(LanternType::from_type(r#type), LanternType::Null)))
                        .collect();

                    let ret = ret.as_ref()
                        .map(|(_, r#type)| sink.emit_or(LanternType::from_type(r#type), LanternType::Null))
                        .unwrap_or(LanternType::Null);

                    scope.insert_function(ident.0.clone(), LanternFunction::new(next_fun_index, args, ret));
                    next_fun_index += 1;
                },
                Item::Struct(_) => todo!(),
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
                            let r#type = compile_expr(condition, &scope, frame, globals, sink)?;
                            if r#type != LanternType::Bool {
                                error!(in sink; condition_span => "expected `bool`, but got {type} instead");
                                // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                            }

                            let false_index = frame.instructions.len();
                            inst!(frame.instructions; GOTO_IF_FALSE 0);

                            let block_scope = scope.child_block();
                            let branch_return = compile_stmts(block.stmts.0, block_scope, frame, globals, sink);
                            match (overall_return, branch_return) {
                                (Some(ControlFlow::Break(_)), ControlFlow::Break(_)) => {},
                                (Some(ControlFlow::Break(_)), ControlFlow::Continue(_)) => overall_return = Some(branch_return),
                                (Some(ControlFlow::Continue(_)), _) => {},
                                (None, _) => overall_return = Some(branch_return),
                            }
                            end_indices.push(frame.instructions.len());
                            inst!(frame.instructions; GOTO 0);
                            frame.instructions[false_index] = Instruction::PopGotoIfFalse(frame.instructions.len());

                            match branch {
                                Some((_, next_branch)) => current_branch = *next_branch,
                                None => break,
                            }
                        },
                        IfBranch::Else(block) => {
                            let block_scope = scope.child_block();
                            let branch_return = compile_stmts(block.stmts.0, block_scope, frame, globals, sink);
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
                    frame.instructions[index] = Instruction::Goto(frame.instructions.len());
                }

                if let Some(ControlFlow::Break(r#type)) = overall_return && has_else {
                    return ControlFlow::Break(r#type);
                }
            },
            Stmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                let condition_span = condition.span();
                let head = frame.instructions.len();

                let r#type = compile_expr(condition, &scope, frame, globals, sink)?;
                if r#type != LanternType::Bool {
                    error!(in sink; condition_span => "expected `bool`, but got {type} instead");
                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                }
                let break_index = frame.instructions.len();
                inst!(frame.instructions; POP_GOTO_IF_FALSE 0);

                let block_scope = scope.child_block();
                // we can't assume the initial condition is met so these may not even be ran
                let _ = compile_stmts(block.stmts.0, block_scope, frame, globals, sink);
                inst!(frame.instructions; GOTO head);

                frame.instructions[break_index] = Instruction::PopGotoIfFalse(frame.instructions.len());
            },
            Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: None, .. }) => {
                // TODO: unitialized vars
                let local_index = frame.declare_local(ident.0.clone());
                if scope.insert_variable(ident.0.clone(), sink.emit_or(LanternType::from_type(&r#type), LanternType::Null)).is_none() {
                    error!(in sink; ident.span() => "variable `{}` already declared", ident.0);
                    // FIXME: CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident), ident_span)?;
                }
                inst! { frame.instructions; 
                    [PUSHU 0]
                    [STORE_LOCAL local_index]
                }
            },
            Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                let init_span = init.span();
                let init_type = compile_expr(init, &scope, frame, globals, sink)?;

                let var_type = sink.emit_or(LanternType::from_type(&r#type), LanternType::Null);
                if var_type != init_type {
                    error!(in sink; init_span => "expected {var_type}, but got {init_type} instead");
                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: var_type, got: init_type }, init_span));
                }
                let local_index = frame.declare_local(ident.0.clone());
                if scope.insert_variable(ident.0.clone(), var_type).is_none() {
                    error!(in sink; ident.span() => "variable `{}` already declared", ident.0);
                    // FIXME: CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident.clone()), ident_span)?;
                }
                inst!(frame.instructions; STORE_LOCAL local_index);
            },
            Stmt::Return(ret_keyword, expr, _) => {
                let expected_ret = match &frame.ret_type {
                    Some(ret) => ret.clone(),
                    _ => {
                        error!(in sink; ret_keyword.span() => "{ret_keyword} not allowed here");
                        return ControlFlow::Break(());
                    },
                };
                let ret = compile_expr(expr, &scope, frame, globals, sink)?;
                if expected_ret != ret {
                    error!(in sink; ret_keyword.span() => "expected {expected_ret}, but got {ret} instead");
                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: expected_ret, got: ret }, span));
                }
                inst!(frame.instructions; RET);
                return ControlFlow::Break(());
            },
            Stmt::Break(Break(_), _) => {
                todo!()
            },
            Stmt::Expr(expr, _) => {
                compile_expr(expr, &scope, frame, globals, sink)?;
                inst!(frame.instructions; POP);
            },
            Stmt::Item(Item::Using(_)) => todo!(),
            Stmt::Item(Item::Fun(ItemFun { args, block, ret, .. })) => {
                let ret = ret
                    .map(|(_, r#type)| sink.emit_or(LanternType::from_type(&r#type), LanternType::Null))
                    .unwrap_or(LanternType::Null);

                let mut fun_scope = scope.child_function(block.open_brace.span());
                let mut fun_frame = StackFrame::new_fun(ret);

                args.0.into_iter()
                    .for_each(|FunArg { ident, r#type, .. }| {
                        match LanternType::from_type(&r#type) {
                            Ok(r#type) => {
                                fun_frame.declare_local(ident.0.clone());
                                if fun_scope.insert_variable(ident.0.clone(), r#type).is_none() {
                                    error!(in sink; ident.span() => "argument `{}` already declared", ident.0);
                                    // FIXME: .ok_or(CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident), span))
                                }
                            },
                            Err(err) => sink.emit(err),
                        }
                    });

                let current_index = globals.funs.len();
                globals.funs.push(GeneratedFunction::Instructions(InstructionSet::default()));
                let _ = compile_stmts(block.stmts.0, fun_scope, &mut fun_frame, globals, sink);

                globals.funs[current_index] = fun_frame.into_gen();
            },
            Stmt::Item(Item::Native(ItemNative { ident, .. })) => {
                let ptr = native::get_native_fn(&ident.0).unwrap_or_else(|| {
                    error!(in sink; ident.span() => "unknown native `{}`", ident.0);
                    // FIXME: CompilerError::new(CompilerErrorKind::UnknownNative(ident), span)
                    fn empty(_: &mut VM, _: [Slot; 256]) -> Result<Slot, RuntimeError> {
                        Ok(Slot::new_primitive(0))
                    }
                    empty as fn(&mut VM, [Slot; 256]) -> Result<Slot, RuntimeError>
                });

                globals.funs.push(GeneratedFunction::Native(ptr));
            },
            Stmt::Item(Item::Struct(_)) => {},
        }
    };

    // implicit return
    match scope.into_kind() {
        ScopeKind::Function(_, span) => {
            let ret_type = frame.ret_type.clone().expect("function scope has return type");
            if ret_type != LanternType::Null {
                error!(in sink; span => "expected function to return {ret_type}");
                // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: ret_type, got: LanternType::Null }, span));
            };
            inst! { frame.instructions;
                [PUSHU 0]
                [RET]
            }
            ControlFlow::Break(())
        },
        ScopeKind::Module => {
            inst! { frame.instructions;
                [PUSHU 0]
                [RET]
            }
            ControlFlow::Break(())
        },
        ScopeKind::Block(_) => ControlFlow::Continue(())
    }
}

fn compile_expr(expression: Expr, scope: &Scope, frame: &mut StackFrame, globals: &mut Globals, sink: &mut DiagnosticSink) -> ControlFlow<(), LanternType> {
    match expression {
        Expr::Literal(Literal::Integer(int, _)) => {
            inst!(frame.instructions; PUSHI int);
            ControlFlow::Continue(LanternType::Integer)
        },
        Expr::Literal(Literal::Float(float, _)) => {
            inst!(frame.instructions; PUSHF float);
            ControlFlow::Continue(LanternType::Float)
        },
        Expr::Literal(Literal::True(_)) => {
            inst!(frame.instructions; PUSHU 1);
            ControlFlow::Continue(LanternType::Bool)
        },
        Expr::Literal(Literal::False(_)) => {
            inst!(frame.instructions; PUSHU 0);
            ControlFlow::Continue(LanternType::Bool)
        },
        Expr::Literal(Literal::String(string, _)) => {
            inst!(frame.instructions; ALLOC_STR string.clone());
            ControlFlow::Continue(LanternType::String)
        },
        Expr::FunCall(ExprFunCall { expr, args, .. }) => {
            let span = expr.span();
            let r#type = compile_expr(*expr, scope, frame, globals, sink)?;
            if let LanternType::Function { args: fun_args, ret } = r#type {
                let fun_args_len = fun_args.len();
                if args.0.len() != fun_args_len {
                    error!(in sink; span => "expected function to have {} args, got {} args instead", fun_args_len, args.0.len());
                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::MismatchedFunctionArgs { expects: fun_args.len(), got: args.0.len() }, span));
                }

                for (expr, r#type) in args.0.into_iter().zip(fun_args) {
                    let expr_span = expr.span();
                    let expr_type = compile_expr(expr, scope, frame, globals, sink)?;
                    if expr_type != r#type {
                        error!(in sink; expr_span => "expected {type}, got {expr_type} instead");
                        // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: r#type, got: expr_type }, expr_span));
                    }
                }

                inst!(frame.instructions; INV fun_args_len);

                ControlFlow::Continue(*ret)
            } else {
                error!(in sink; span => "expected function");
                ControlFlow::Continue(LanternType::Null)
                // FIXME: Err(CompilerError::new(CompilerErrorKind::TypeError { expected, got: r#type }, span))
            }
        },
        Expr::Binary(ExprBinary { lhs, op, rhs }) => {
            // special cases
            match op {
                BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                    let lhs_type = compile_expr(*lhs, scope, frame, globals, sink)?;
                    let goto_index = frame.instructions.len();

                    match op {
                        BinaryOperator::And(_) => inst!(frame.instructions; GOTO_IF_FALSE 0),
                        BinaryOperator::Or(_) => inst!(frame.instructions; GOTO_IF_TRUE 0),
                        _ => unreachable!(),
                    };
                    inst!(frame.instructions; POP);

                    let rhs_type = compile_expr(*rhs, scope, frame, globals, sink)?;

                    let goto_inst = match op {
                        BinaryOperator::And(_) => Instruction::GotoIfFalse(frame.instructions.len()),
                        BinaryOperator::Or(_) => Instruction::GotoIfTrue(frame.instructions.len()),
                        _ => unreachable!(),
                    };
                    frame.instructions[goto_index] = goto_inst;

                    if (lhs_type.clone(), rhs_type.clone()) != (LanternType::Bool, LanternType::Bool) {
                        error!(in sink; op.span() => "{op} cannot be applied to {lhs_type} and {rhs_type}");
                        // FIXME: return Err(CompilerError::new(CompilerErrorKind::BinaryOperator { op, got: (lhs_type, rhs_type) }, span));
                    }

                    return ControlFlow::Continue(LanternType::Bool);
                },
                BinaryOperator::Assign(punct) => {
                    let rhs_span = rhs.span();
                    match *lhs {
                        Expr::Identifier(ident) => {
                            let Some(var) = scope.variable(&ident.0) else {
                                error!(in sink; ident.span() => "unknown variable `{}`", ident.0);
                                // FIXME: CompilerError::new(CompilerErrorKind::UnknownVariable(ident.clone()), ident.span())?;
                                return ControlFlow::Continue(LanternType::Null);
                            };

                            let rhs = compile_expr(*rhs, scope, frame, globals, sink)?;

                            if var.r#type != rhs {
                                error!(in sink; rhs_span => "expected {}, but got {rhs} instead", var.r#type);
                                // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: var.r#type, got: r#type }, ident.span()));
                            }
                            let local_index = frame.find_local(&ident.0).expect("local var exists");
                            inst!(frame.instructions; STORE_LOCAL local_index);
                        },
                        Expr::Index(ExprIndex { expr, index, .. }) => {
                            // TODO: method for indexing
                            let expr_span = expr.span();
                            let r#type = compile_expr(*expr, scope, frame, globals, sink)?;
                            let inner = match r#type {
                                LanternType::Array(inner) => *inner,
                                LanternType::String => LanternType::Integer,
                                _ => {
                                    error!(in sink; expr_span => "expected array or string");
                                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Array(Box::new(LanternType::Null)), got: r#type }, expr_span));
                                    LanternType::Null
                                },
                            };

                            let rhs = compile_expr(*rhs, scope, frame, globals, sink)?;

                            if rhs != inner {
                                error!(in sink; rhs_span => "expected {inner}, but got {rhs} instead");
                            }

                            let index_span = index.span();
                            let index_type = compile_expr(*index, scope, frame, globals, sink)?;
                            if index_type != LanternType::Integer {
                                error!(in sink; index_span => "expected index to be an integer");
                                // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Integer, got: index_type }, index_span));
                            }

                            inst!(frame.instructions; WRITE_INDEX);
                        },
                        Expr::Field(_) => todo!(),
                        _ => {
                            error!(in sink; punct.span() => "bad left-hand-side of assignment");
                            // FIXME: return Err(CompilerError::new(CompilerErrorKind::BadLeftHandSide, punct.span()));
                        },
                    };
                    return ControlFlow::Continue(LanternType::Null);
                },
                _ => {},
            }

            let lhs = compile_expr(*lhs, scope, frame, globals, sink)?;
            let rhs = compile_expr(*rhs, scope, frame, globals, sink)?;

            match (lhs, op, rhs) {
                (LanternType::Float, BinaryOperator::Add(_), LanternType::Float) => {
                    inst!(frame.instructions; ADDF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (LanternType::Integer, BinaryOperator::Add(_), LanternType::Integer) => {
                    inst!(frame.instructions; ADDI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (LanternType::Float, BinaryOperator::Sub(_), LanternType::Float) => {
                    inst!(frame.instructions; SUBF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (LanternType::Integer, BinaryOperator::Sub(_), LanternType::Integer) => {
                    inst!(frame.instructions; SUBI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (LanternType::Float, BinaryOperator::Mult(_), LanternType::Float) => {
                    inst!(frame.instructions; MULTF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (LanternType::Integer, BinaryOperator::Mult(_), LanternType::Integer) => {
                    inst!(frame.instructions; MULTI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (LanternType::Float, BinaryOperator::Div(_), LanternType::Float) => {
                    inst!(frame.instructions; DIVF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (LanternType::Integer, BinaryOperator::Div(_), LanternType::Integer) => {
                    inst!(frame.instructions; DIVI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (LanternType::Float, BinaryOperator::Mod(_), LanternType::Float) => {
                    inst!(frame.instructions; MODF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (LanternType::Integer, BinaryOperator::Mod(_), LanternType::Integer) => {
                    inst!(frame.instructions; MODI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (LanternType::Float, BinaryOperator::Lt(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_LT);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Integer, BinaryOperator::Lt(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_LT);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Float, BinaryOperator::Le(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_LE);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Integer, BinaryOperator::Le(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_LE);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Float, BinaryOperator::Gt(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_GT);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Integer, BinaryOperator::Gt(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_GT);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Float, BinaryOperator::Ge(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_GE);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Integer, BinaryOperator::Ge(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_GE);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Float, BinaryOperator::Eq(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_EQ);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (LanternType::Integer, BinaryOperator::Eq(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_EQ);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (_, BinaryOperator::Assign(_) | BinaryOperator::And(_) | BinaryOperator::Or(_), _) => unreachable!(),
                (lhs, op, rhs) => {
                    error!(in sink; op.span() => "{op} cannot be applied to {lhs} and {rhs}");
                    // FIXME: Err(CompilerError::new(CompilerErrorKind::BinaryOperator { op, got: (lhs, rhs) }, span))
                    ControlFlow::Continue(LanternType::Null)
                },
            }
        },
        Expr::Unary(ExprUnary { op, expr }) => {
            let r#type = compile_expr(*expr, scope, frame, globals, sink)?;
            match (op, r#type) {
                (UnaryOperator::Negate(_), LanternType::Float) => {
                    inst!(frame.instructions; NEGF);
                    ControlFlow::Continue(LanternType::Float)
                },
                (UnaryOperator::Negate(_), LanternType::Integer) => {
                    inst!(frame.instructions; NEGI);
                    ControlFlow::Continue(LanternType::Integer)
                },
                (UnaryOperator::Not(_), LanternType::Bool) => {
                    inst!(frame.instructions; NOT);
                    ControlFlow::Continue(LanternType::Bool)
                },
                (op, got) => {
                    error!(in sink; op.span() => "{op} cannot be applied to {got}");
                    // FIXME: Err(CompilerError::new(CompilerErrorKind::UnaryOperator { op, got }, span))
                    ControlFlow::Continue(LanternType::Null)
                },
            }
        },
        Expr::Paren(ExprParen { expr, .. }) => compile_expr(*expr, scope, frame, globals, sink),
        Expr::Block(ExprBlock { stmts, .. }) => {
            let block_scope = scope.child_block();
            compile_stmts(stmts.0, block_scope, frame, globals, sink)?;
            inst!(frame.instructions; PUSHU 0);
            ControlFlow::Continue(LanternType::Null)
        },
        Expr::Array(ExprArray { elements, .. }) => {
            let len = elements.0.len();
            let mut inner = None;

            for expr in elements.0 {
                let span = expr.span();
                inner = match (inner, compile_expr(expr, scope, frame, globals, sink)?) {
                    (None, r#type) => Some(r#type),
                    (Some(r#type), expr_type) if r#type == expr_type => Some(r#type),
                    (Some(r#type), expr_type) => {
                        error!(in sink; span => "expected {type}, but got {expr_type} instead");
                        // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: r#type, got: expr_type }, span)),
                        Some(r#type)
                    },
                }
            }

            inst! { frame.instructions;
                [PUSHU 1]
                [ALLOC_ARR len]
            }
            match inner {
                Some(inner) => ControlFlow::Continue(LanternType::Array(Box::new(inner))),
                // TODO: type hint
                None => ControlFlow::Continue(LanternType::Array(Box::new(LanternType::Null))),
            }
        },
        Expr::Index(ExprIndex { expr, index, .. }) => {
            let expr_span = expr.span();
            let r#type = compile_expr(*expr, scope, frame, globals, sink)?;
            let inner = match r#type {
                LanternType::Array(inner) => *inner,
                LanternType::String => LanternType::Integer,
                _ => {
                    error!(in sink; expr_span => "expected array or string");
                    // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Array(Box::new(LanternType::Null)), got: r#type }, expr_span));
                    LanternType::Null
                },
            };
            let index_span = index.span();
            let index_type = compile_expr(*index, scope, frame, globals, sink)?;
            if index_type != LanternType::Integer {
                error!(in sink; index_span => "expected index to be an integer");
                // FIXME: return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Integer, got: index_type }, index_span));
            }

            inst!(frame.instructions; INDEX);
            ControlFlow::Continue(inner)
        },
        Expr::Identifier(ident) => {
            let span = ident.span();
            match scope.variable(&ident.0) {
                Some(var) => {
                    let local_index = frame.find_local(&ident.0).expect("local var exists");
                    inst!(frame.instructions; LOAD_LOCAL local_index);
                    ControlFlow::Continue(var.r#type)
                },
                None => {
                    let Some(fun) = scope.function(&ident.0) else {
                        error!(in sink; span => "unknown variable `{}`", ident.0);
                        // FIXME: CompilerError::new(CompilerErrorKind::UnknownVariable(ident), span)?;
                        return ControlFlow::Continue(LanternType::Null);
                    };
                    inst!(frame.instructions; PUSHU fun.index as u64);
                    ControlFlow::Continue(LanternType::Function { args: fun.args.iter().map(|(_, r#type)| r#type.clone()).collect(), ret: Box::new(fun.ret.clone()) })
                }
            }
        },
        Expr::Field(_) => todo!(),
    }
}

#[derive(Debug, Clone)]
pub struct LanternFunction {
    pub index: usize,
    pub args: Vec<(Ident, LanternType)>,
    pub ret: LanternType,
}

impl LanternFunction {
    pub fn new(index: usize, args: Vec<(Ident, LanternType)>, ret: LanternType) -> Self {
        Self { index, args, ret }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternStruct {
    pub fields: Box<[StructField]>,
    pub size: usize,
}

impl LanternStruct {
    pub fn new(fields: Box<[(String, LanternType)]>) -> Self {
        let alignment = fields.iter()
            .map(|(_, r#type)| r#type.alignment())
            .max()
            .unwrap_or(1);
        let size = if fields.is_empty() {
            0
        } else {
            alignment * fields.len() - 1 + fields.last().map(|(_, r#type)| r#type.size()).unwrap_or(0)
        };
        let fields = fields.into_iter()
            .enumerate()
            .map(|(i, (name, r#type))| StructField {
                name,
                r#type,
                offset: i * alignment,
            })
            .collect();

        Self {
            fields,
            size,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub offset: usize,
    pub r#type: LanternType,
}

#[derive(Debug, Clone, PartialEq)]
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
    Instructions(InstructionSet),
    Native(NativeFn),
}

impl Display for GeneratedFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            Self::Instructions(instructions) => instructions.fmt(f)?,
            Self::Native(ptr) => writeln!(f, "<native function @ {ptr:?}>")?,
        };
        Ok(())
    }
}

