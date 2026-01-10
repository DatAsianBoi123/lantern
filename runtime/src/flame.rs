use std::{fmt::{Display, Formatter}, mem::MaybeUninit, rc::Rc};

use instruction::InstructionSet;
use parse::{Boolean, FunArg, Ident, IfBranch, IfStmt, Item, ItemFun, ItemNative, LanternFile, Literal, QuotedString, Reassign, Stmt, ValDeclaration, WhileStmt, error::Span, expr::{BinaryOperator, Expr, ExprBinary, ExprBlock, ExprFunCall, ExprParen, ExprUnary, UnaryOperator}, keyword::{Break, Return}};

use crate::{Slot, VM, error::RuntimeError, flame::{error::{CompilerError, CompilerErrorKind}, instruction::Instruction, scope::{Scope, ScopeKind, StackFrame}, r#type::LanternType}, inst};

pub type GenerateFunPtr = Rc<MaybeUninit<GeneratedFunction>>;
pub type NativeFn = fn(&mut VM, [Slot; 256]) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod error;
pub mod r#type;
pub mod scope;
pub mod native;

pub fn ignite(file: LanternFile, funs: &mut Vec<GeneratedFunction>) -> Result<GeneratedFunction, CompilerError> {
    let mut frame = StackFrame::new_module();
    compile_stmts(file.stmts, Scope::new(), &mut frame, funs)?;
    Ok(frame.into_gen())
}

fn compile_stmts(statements: Vec<Stmt>, mut scope: Scope, frame: &mut StackFrame, funs: &mut Vec<GeneratedFunction>) -> Result<(), CompilerError> {
    let mut next_fun_index = funs.len();
    statements.iter()
        .filter_map(|statement| {
            if let Stmt::Item(item) = statement {
                Some(item)
            } else {
                None
            }
        })
        .try_for_each(|item| {
            match item {
                Item::Using(_) => todo!(),
                Item::Fun(ItemFun { ident, args, ret, .. }) => {
                    let args = args.0.iter()
                        .map(|FunArg { ident, r#type, .. }| LanternType::from_type(r#type).map(|r#type| (ident.clone(), r#type)))
                        .collect::<Result<_, _>>()?;

                    let ret = match ret {
                        Some((_, path)) => LanternType::from_type(path)?,
                        None => LanternType::Null,
                    };

                    scope.insert_function(ident.0.clone(), LanternFunction::new(next_fun_index, args, ret));
                    next_fun_index += 1;
                },
                Item::Native(ItemNative { ident, args, ret, .. }) => {
                    let args = args.0.iter()
                        .map(|FunArg { ident, r#type, .. }| LanternType::from_type(r#type).map(|r#type| (ident.clone(), r#type)))
                        .collect::<Result<_, _>>()?;

                    let ret = match ret {
                        Some((_, path)) => LanternType::from_type(path)?,
                        None => LanternType::Null,
                    };

                    scope.insert_function(ident.0.clone(), LanternFunction::new(next_fun_index, args, ret));
                    next_fun_index += 1;
                },
                Item::Struct(_) => todo!(),
            };
            Ok(())
        })?;

    for statement in statements {
        match statement {
            Stmt::Comment(_, _) => {},
            Stmt::IfStmt(if_stmt) => {
                let mut end_indices = Vec::new();
                let mut current_branch = IfBranch::ElseIf(if_stmt);

                loop {
                    match current_branch {
                        IfBranch::ElseIf(IfStmt { condition, block, branch, .. }) => {
                            let condition_span = condition.span().clone();
                            let r#type = compile_expr(condition, &scope, frame, funs)?;
                            if r#type != LanternType::Bool {
                                return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                            }

                            let false_index = frame.instructions.len();
                            inst!(frame.instructions; GOTO_IF_FALSE 0);

                            let block_scope = scope.child_block();
                            compile_stmts(block.stmts, block_scope, frame, funs)?;
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
                            compile_stmts(block.stmts, block_scope, frame, funs)?;
                            break;
                        },
                    }
                }

                for index in end_indices {
                    frame.instructions[index] = Instruction::Goto(frame.instructions.len());
                }
            },
            Stmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                let condition_span = condition.span().clone();
                let head = frame.instructions.len();

                let r#type = compile_expr(condition, &scope, frame, funs)?;
                if r#type != LanternType::Bool {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                }
                let break_index = frame.instructions.len();
                inst!(frame.instructions; POP_GOTO_IF_FALSE 0);

                let block_scope = scope.child_block();
                compile_stmts(block.stmts, block_scope, frame, funs)?;
                inst!(frame.instructions; GOTO head);

                frame.instructions[break_index] = Instruction::PopGotoIfFalse(frame.instructions.len());
            },
            Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: None, .. }) => {
                // TODO: unitialized vars
                let ident_span = ident.1.clone();
                let local_index = frame.declare_local(ident.0.clone());
                scope.insert_variable(ident.0.clone(), LanternType::from_type(&r#type)?)
                    .ok_or(CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident), ident_span))?;
                inst! { frame.instructions; 
                    [PUSHU 0]
                    [STORE_LOCAL local_index]
                }
            },
            Stmt::ValDeclaration(ValDeclaration { ident, r#type, init: Some((_, init)), .. }) => {
                let ident_span = ident.1.clone();
                let init_span = init.span().clone();
                let init_type = compile_expr(init, &scope, frame, funs)?;

                let var_type = LanternType::from_type(&r#type)?;
                if var_type != init_type {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: var_type, got: init_type }, init_span));
                }
                let local_index = frame.declare_local(ident.0.clone());
                scope.insert_variable(ident.0.clone(), var_type)
                    .ok_or(CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident.clone()), ident_span))?;
                inst!(frame.instructions; STORE_LOCAL local_index);
            },
            Stmt::Reassign(Reassign { ident, expr, .. }) => {
                let var = scope.variable(&ident.0)
                    .ok_or(CompilerError::new(CompilerErrorKind::UnknownVariable(ident.clone()), ident.1.clone()))?;

                let r#type = compile_expr(expr, &scope, frame, funs)?;
                if var.r#type != r#type {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: var.r#type, got: r#type }, ident.1.clone()));
                }
                let local_index = frame.find_local(&ident.0).expect("local var exists");
                inst!(frame.instructions; STORE_LOCAL local_index);
            },
            Stmt::Return(Return(span), expr, _) => {
                let expected_ret = match &frame.ret_type {
                    Some(ret) => ret.clone(),
                    _ => return Err(CompilerError::new(CompilerErrorKind::BadReturn, span)),
                };
                let ret = compile_expr(expr, &scope, frame, funs)?;
                if expected_ret != ret {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: expected_ret.clone(), got: ret }, span));
                }
                inst!(frame.instructions; RET);
                return Ok(());
            },
            Stmt::Break(Break(_), _) => {
                todo!()
            },
            Stmt::Expr(expr, _) => {
                compile_expr(expr, &scope, frame, funs)?;
                inst!(frame.instructions; POP);
            },
            Stmt::Item(Item::Using(_)) => todo!(),
            Stmt::Item(Item::Fun(ItemFun { args, block, ret, .. })) => {
                let ret = ret
                    .map(|(_, path)| LanternType::from_type(&path))
                    .unwrap_or(Ok(LanternType::Null))?;

                let mut fun_scope = scope.child_function();
                let mut fun_frame = StackFrame::new_fun(ret);

                args.0.into_iter()
                    .try_for_each(|FunArg { ident, r#type, .. }| {
                        LanternType::from_type(&r#type)
                            .and_then(|r#type| {
                                let span = ident.1.clone();
                                fun_frame.declare_local(ident.0.clone());
                                fun_scope.insert_variable(ident.0.clone(), r#type)
                                    .map(|_| ())
                                    .ok_or(CompilerError::new(CompilerErrorKind::ItemAlreadyDeclared(ident), span))
                            })
                    })?;

                let current_index = funs.len();
                funs.push(GeneratedFunction::Instructions(InstructionSet::default()));
                compile_stmts(block.stmts, fun_scope, &mut fun_frame, funs)?;

                funs[current_index] = fun_frame.into_gen();
            },
            Stmt::Item(Item::Native(ItemNative { ident, .. })) => {
                let ptr = native::get_native_fn(&ident.0).ok_or_else(|| {
                    let span = ident.1.clone();
                    CompilerError::new(CompilerErrorKind::UnknownNative(ident.clone()), span)
                })?;

                funs.push(GeneratedFunction::Native(ptr));
            },
            Stmt::Item(Item::Struct(_)) => {},
        }
    };
    // TODO: handle branches
    if !matches!(scope.kind(), ScopeKind::Block { .. }) {
        if let Some(ret_type) = &frame.ret_type && *ret_type != LanternType::Null {
            // TODO: span
            return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: ret_type.clone(), got: LanternType::Null }, Span::new(0, 0)));
        };
        inst!(frame.instructions; PUSHU 0);
        inst!(frame.instructions; RET);
    }

    Ok(())
}

fn compile_expr(expression: Expr, scope: &Scope, frame: &mut StackFrame, funs: &mut Vec<GeneratedFunction>) -> Result<LanternType, CompilerError> {
    match expression {
        Expr::Literal(Literal::Number(number)) => {
            inst!(frame.instructions; PUSHF number.0);
            Ok(LanternType::Number)
        },
        Expr::Literal(Literal::Boolean(bool)) => {
            match bool {
                Boolean::True(_) => inst!(frame.instructions; PUSHU 1),
                Boolean::False(_) => inst!(frame.instructions; PUSHU 0),
            }
            Ok(LanternType::Bool)
        },
        Expr::Literal(Literal::String(QuotedString(string, _))) => {
            inst!(frame.instructions; ALLOC_STR string.clone());
            Ok(LanternType::String)
        },
        Expr::FunCall(ExprFunCall { expr, args, .. }) => {
            let span = expr.span().clone();
            let r#type = compile_expr(*expr, scope, frame, funs)?;
            if let LanternType::Function { args: fun_args, ret } = r#type {
                let fun_args_len = fun_args.len();
                if args.0.len() != fun_args_len {
                    return Err(CompilerError::new(CompilerErrorKind::MismatchedFunctionArgs { expects: fun_args.len(), got: args.0.len() }, span));
                }

                for (expr, r#type) in args.0.into_iter().zip(fun_args) {
                    let expr_span = expr.span().clone();
                    let expr_type = compile_expr(expr, scope, frame, funs)?;
                    if expr_type != r#type {
                        return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: r#type.clone(), got: expr_type }, expr_span));
                    }
                }

                inst!(frame.instructions; INV fun_args_len);

                Ok(*ret)
            } else {
                // TODO: type hint
                let expected = LanternType::Function { args: Vec::new(), ret: Box::new(LanternType::Null) };
                Err(CompilerError::new(CompilerErrorKind::TypeError { expected, got: r#type }, span))
            }
        },
        Expr::Binary(ExprBinary { lhs, op, rhs }) => {
            match op {
                BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                    let lhs_type = compile_expr(*lhs, scope, frame, funs)?;
                    let goto_index = frame.instructions.len();

                    match op {
                        BinaryOperator::And(_) => inst!(frame.instructions; GOTO_IF_FALSE 0),
                        BinaryOperator::Or(_) => inst!(frame.instructions; GOTO_IF_TRUE 0),
                        _ => unreachable!(),
                    };
                    inst!(frame.instructions; POP);

                    let rhs_type = compile_expr(*rhs, scope, frame, funs)?;

                    let goto_inst = match op {
                        BinaryOperator::And(_) => Instruction::GotoIfFalse(frame.instructions.len()),
                        BinaryOperator::Or(_) => Instruction::GotoIfTrue(frame.instructions.len()),
                        _ => unreachable!(),
                    };
                    frame.instructions[goto_index] = goto_inst;

                    if (lhs_type.clone(), rhs_type.clone()) != (LanternType::Bool, LanternType::Bool) {
                        let span = op.span().clone();
                        return Err(CompilerError::new(CompilerErrorKind::BinaryOperator { op, got: (lhs_type, rhs_type) }, span));
                    }

                    return Ok(LanternType::Bool);
                },
                _ => {},
            }

            let lhs = compile_expr(*lhs, scope, frame, funs)?;
            let rhs = compile_expr(*rhs, scope, frame, funs)?;

            match (lhs, op, rhs) {
                (LanternType::Number, BinaryOperator::Add(_), LanternType::Number) => {
                    inst!(frame.instructions; ADDF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Sub(_), LanternType::Number) => {
                    inst!(frame.instructions; SUBF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Mult(_), LanternType::Number) => {
                    inst!(frame.instructions; MULTF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Div(_), LanternType::Number) => {
                    inst!(frame.instructions; DIVF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Mod(_), LanternType::Number) => {
                    inst!(frame.instructions; MODF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Lt(_), LanternType::Number) => {
                    inst!(frame.instructions; COMP_LT);
                    Ok(LanternType::Bool)
                },
                (LanternType::Number, BinaryOperator::Le(_), LanternType::Number) => {
                    inst!(frame.instructions; COMP_LE);
                    Ok(LanternType::Bool)
                },
                (LanternType::Number, BinaryOperator::Gt(_), LanternType::Number) => {
                    inst!(frame.instructions; COMP_GT);
                    Ok(LanternType::Bool)
                },
                (LanternType::Number, BinaryOperator::Ge(_), LanternType::Number) => {
                    inst!(frame.instructions; COMP_GE);
                    Ok(LanternType::Bool)
                },
                (LanternType::Number, BinaryOperator::Eq(_), LanternType::Number) => {
                    inst!(frame.instructions; COMP_EQ);
                    Ok(LanternType::Bool)
                },
                (_, BinaryOperator::And(_) | BinaryOperator::Or(_), _) => unreachable!(),
                (lhs, op, rhs) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::BinaryOperator { op, got: (lhs, rhs) }, span))
                },
            }
        },
        Expr::Unary(ExprUnary { op, expr }) => {
            match (op, compile_expr(*expr, scope, frame, funs)?) {
                (UnaryOperator::Negate(_), LanternType::Number) => {
                    inst!(frame.instructions; NEGF);
                    Ok(LanternType::Number)
                },
                (UnaryOperator::Not(_), LanternType::Bool) => {
                    inst!(frame.instructions; NOT);
                    Ok(LanternType::Bool)
                },
                (op, got) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::UnaryOperator { op, got }, span))
                },
            }
        },
        Expr::Paren(ExprParen { expr, .. }) => compile_expr(*expr, scope, frame, funs),
        Expr::Block(ExprBlock { stmts, .. }) => {
            let block_scope = scope.child_block();
            compile_stmts(stmts, block_scope, frame, funs)?;
            inst!(frame.instructions; PUSHU 0);
            Ok(LanternType::Null)
        },
        Expr::Identifier(ident) => {
            let span = ident.1.clone();
            match scope.variable(&ident.0) {
                Some(var) => {
                    let local_index = frame.find_local(&ident.0).expect("local var exists");
                    inst!(frame.instructions; LOAD_LOCAL local_index);
                    Ok(var.r#type)
                },
                None => {
                    let fun = scope.function(&ident.0)
                        .ok_or(CompilerError::new(CompilerErrorKind::UnknownVariable(ident), span))?;
                    inst!(frame.instructions; PUSHU fun.index as u64);
                    Ok(LanternType::Function { args: fun.args.iter().map(|(_, r#type)| r#type.clone()).collect(), ret: Box::new(fun.ret.clone()) })
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

