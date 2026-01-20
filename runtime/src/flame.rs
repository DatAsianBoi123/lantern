use std::{fmt::{Display, Formatter}, mem::MaybeUninit, ops::ControlFlow, rc::Rc};

use instruction::InstructionSet;
use parse::{Boolean, FunArg, Ident, IfBranch, IfStmt, Item, ItemFun, ItemNative, LanternFile, Literal, Number, QuotedString, Reassign, Stmt, ValDeclaration, WhileStmt, expr::{BinaryOperator, Expr, ExprArray, ExprBinary, ExprBlock, ExprFunCall, ExprIndex, ExprParen, ExprUnary, UnaryOperator}, keyword::{Break, Return}};

use crate::{Slot, VM, error::RuntimeError, flame::{error::{CompilerError, CompilerErrorKind}, instruction::Instruction, scope::{Globals, Scope, ScopeKind, StackFrame}, r#type::LanternType}, inst};

pub type GenerateFunPtr = Rc<MaybeUninit<GeneratedFunction>>;
pub type NativeFn = fn(&mut VM, [Slot; 256]) -> Result<Slot, RuntimeError>;

pub mod instruction;
pub mod error;
pub mod r#type;
pub mod scope;
pub mod native;

macro_rules! short_curcuit {
    ($expr: expr) => {
        match $expr {
            ControlFlow::Break(val) => return Ok(ControlFlow::Break(val)),
            ControlFlow::Continue(val) => val,
        }
    };
}

pub fn ignite(file: LanternFile, globals: &mut Globals) -> Result<GeneratedFunction, CompilerError> {
    let mut frame = StackFrame::new_module();
    let _ = compile_stmts(file.stmts, Scope::new(), &mut frame, globals)?;
    Ok(frame.into_gen())
}

fn compile_stmts(statements: Vec<Stmt>, mut scope: Scope, frame: &mut StackFrame, globals: &mut Globals) -> Result<ControlFlow<()>, CompilerError> {
    let mut next_fun_index = globals.funs.len();
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
                let mut overall_return = None;
                let mut has_else = false;

                loop {
                    match current_branch {
                        IfBranch::ElseIf(IfStmt { condition, block, branch, .. }) => {
                            let condition_span = condition.span().clone();
                            let r#type = short_curcuit!(compile_expr(condition, &scope, frame, globals)?);
                            if r#type != LanternType::Bool {
                                return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                            }

                            let false_index = frame.instructions.len();
                            inst!(frame.instructions; GOTO_IF_FALSE 0);

                            let block_scope = scope.child_block();
                            let branch_return = compile_stmts(block.stmts, block_scope, frame, globals)?;
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
                            let branch_return = compile_stmts(block.stmts, block_scope, frame, globals)?;
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
                    return Ok(ControlFlow::Break(r#type));
                }
            },
            Stmt::WhileStmt(WhileStmt { condition, block, .. }) => {
                let condition_span = condition.span().clone();
                let head = frame.instructions.len();

                let r#type = short_curcuit!(compile_expr(condition, &scope, frame, globals)?);
                if r#type != LanternType::Bool {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Bool, got: r#type }, condition_span));
                }
                let break_index = frame.instructions.len();
                inst!(frame.instructions; POP_GOTO_IF_FALSE 0);

                let block_scope = scope.child_block();
                // we can't assume the initial condition is met so these may not even be run
                let _ = compile_stmts(block.stmts, block_scope, frame, globals)?;
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
                let init_type = short_curcuit!(compile_expr(init, &scope, frame, globals)?);

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

                let r#type = short_curcuit!(compile_expr(expr, &scope, frame, globals)?);
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
                let ret = short_curcuit!(compile_expr(expr, &scope, frame, globals)?);
                if expected_ret != ret {
                    return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: expected_ret.clone(), got: ret }, span));
                }
                inst!(frame.instructions; RET);
                return Ok(ControlFlow::Break(()));
            },
            Stmt::Break(Break(_), _) => {
                todo!()
            },
            Stmt::Expr(expr, _) => {
                short_curcuit!(compile_expr(expr, &scope, frame, globals)?);
                inst!(frame.instructions; POP);
            },
            Stmt::Item(Item::Using(_)) => todo!(),
            Stmt::Item(Item::Fun(ItemFun { args, block, ret, .. })) => {
                let ret = ret
                    .map(|(_, path)| LanternType::from_type(&path))
                    .unwrap_or(Ok(LanternType::Null))?;

                let mut fun_scope = scope.child_function(block.open_brace.0.clone());
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

                let current_index = globals.funs.len();
                globals.funs.push(GeneratedFunction::Instructions(InstructionSet::default()));
                let _ = compile_stmts(block.stmts, fun_scope, &mut fun_frame, globals)?;

                globals.funs[current_index] = fun_frame.into_gen();
            },
            Stmt::Item(Item::Native(ItemNative { ident, .. })) => {
                let ptr = native::get_native_fn(&ident.0).ok_or_else(|| {
                    let span = ident.1.clone();
                    CompilerError::new(CompilerErrorKind::UnknownNative(ident.clone()), span)
                })?;

                globals.funs.push(GeneratedFunction::Native(ptr));
            },
            Stmt::Item(Item::Struct(_)) => {},
        }
    };

    match scope.into_kind() {
        ScopeKind::Function(_, span) => {
            let ret_type = frame.ret_type.clone().expect("function scope has return type");
            if ret_type != LanternType::Null {
                return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: ret_type.clone(), got: LanternType::Null }, span));
            };
            inst! { frame.instructions;
                [PUSHU 0]
                [RET]
            }
            Ok(ControlFlow::Break(()))
        },
        ScopeKind::Module => {
            inst! { frame.instructions;
                [PUSHU 0]
                [RET]
            }
            Ok(ControlFlow::Break(()))
        },
        ScopeKind::Block(_) => Ok(ControlFlow::Continue(()))
    }
}

fn compile_expr(expression: Expr, scope: &Scope, frame: &mut StackFrame, globals: &mut Globals) -> Result<ControlFlow<(), LanternType>, CompilerError> {
    match expression {
        Expr::Literal(Literal::Number(Number::Integer(int, _))) => {
            inst!(frame.instructions; PUSHI int);
            Ok(ControlFlow::Continue(LanternType::Integer))
        },
        Expr::Literal(Literal::Number(Number::Float(float, _))) => {
            inst!(frame.instructions; PUSHF float);
            Ok(ControlFlow::Continue(LanternType::Float))
        },
        Expr::Literal(Literal::Boolean(bool)) => {
            match bool {
                Boolean::True(_) => inst!(frame.instructions; PUSHU 1),
                Boolean::False(_) => inst!(frame.instructions; PUSHU 0),
            }
            Ok(ControlFlow::Continue(LanternType::Bool))
        },
        Expr::Literal(Literal::String(QuotedString(string, _))) => {
            inst!(frame.instructions; ALLOC_STR string.clone());
            Ok(ControlFlow::Continue(LanternType::String))
        },
        Expr::FunCall(ExprFunCall { expr, args, .. }) => {
            let span = expr.span().clone();
            let r#type = short_curcuit!(compile_expr(*expr, scope, frame, globals)?);
            if let LanternType::Function { args: fun_args, ret } = r#type {
                let fun_args_len = fun_args.len();
                if args.0.len() != fun_args_len {
                    return Err(CompilerError::new(CompilerErrorKind::MismatchedFunctionArgs { expects: fun_args.len(), got: args.0.len() }, span));
                }

                for (expr, r#type) in args.0.into_iter().zip(fun_args) {
                    let expr_span = expr.span().clone();
                    let expr_type = short_curcuit!(compile_expr(expr, scope, frame, globals)?);
                    if expr_type != r#type {
                        return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: r#type.clone(), got: expr_type }, expr_span));
                    }
                }

                inst!(frame.instructions; INV fun_args_len);

                Ok(ControlFlow::Continue(*ret))
            } else {
                // TODO: type hint
                let expected = LanternType::Function { args: Vec::new(), ret: Box::new(LanternType::Null) };
                Err(CompilerError::new(CompilerErrorKind::TypeError { expected, got: r#type }, span))
            }
        },
        Expr::Binary(ExprBinary { lhs, op, rhs }) => {
            match op {
                BinaryOperator::And(_) | BinaryOperator::Or(_) => {
                    let lhs_type = short_curcuit!(compile_expr(*lhs, scope, frame, globals)?);
                    let goto_index = frame.instructions.len();

                    match op {
                        BinaryOperator::And(_) => inst!(frame.instructions; GOTO_IF_FALSE 0),
                        BinaryOperator::Or(_) => inst!(frame.instructions; GOTO_IF_TRUE 0),
                        _ => unreachable!(),
                    };
                    inst!(frame.instructions; POP);

                    let rhs_type = short_curcuit!(compile_expr(*rhs, scope, frame, globals)?);

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

                    return Ok(ControlFlow::Continue(LanternType::Bool));
                },
                _ => {},
            }

            let lhs = short_curcuit!(compile_expr(*lhs, scope, frame, globals)?);
            let rhs = short_curcuit!(compile_expr(*rhs, scope, frame, globals)?);

            match (lhs, op, rhs) {
                (LanternType::Float, BinaryOperator::Add(_), LanternType::Float) => {
                    inst!(frame.instructions; ADDF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (LanternType::Integer, BinaryOperator::Add(_), LanternType::Integer) => {
                    inst!(frame.instructions; ADDI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (LanternType::Float, BinaryOperator::Sub(_), LanternType::Float) => {
                    inst!(frame.instructions; SUBF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (LanternType::Integer, BinaryOperator::Sub(_), LanternType::Integer) => {
                    inst!(frame.instructions; SUBI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (LanternType::Float, BinaryOperator::Mult(_), LanternType::Float) => {
                    inst!(frame.instructions; MULTF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (LanternType::Integer, BinaryOperator::Mult(_), LanternType::Integer) => {
                    inst!(frame.instructions; MULTI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (LanternType::Float, BinaryOperator::Div(_), LanternType::Float) => {
                    inst!(frame.instructions; DIVF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (LanternType::Integer, BinaryOperator::Div(_), LanternType::Integer) => {
                    inst!(frame.instructions; DIVI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (LanternType::Float, BinaryOperator::Mod(_), LanternType::Float) => {
                    inst!(frame.instructions; MODF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (LanternType::Integer, BinaryOperator::Mod(_), LanternType::Integer) => {
                    inst!(frame.instructions; MODI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (LanternType::Float, BinaryOperator::Lt(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_LT);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Integer, BinaryOperator::Lt(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_LT);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Float, BinaryOperator::Le(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_LE);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Integer, BinaryOperator::Le(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_LE);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Float, BinaryOperator::Gt(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_GT);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Integer, BinaryOperator::Gt(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_GT);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Float, BinaryOperator::Ge(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_GE);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Integer, BinaryOperator::Ge(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_GE);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Float, BinaryOperator::Eq(_), LanternType::Float) => {
                    inst!(frame.instructions; FCOMP_EQ);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (LanternType::Integer, BinaryOperator::Eq(_), LanternType::Integer) => {
                    inst!(frame.instructions; ICOMP_EQ);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (_, BinaryOperator::And(_) | BinaryOperator::Or(_), _) => unreachable!(),
                (lhs, op, rhs) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::BinaryOperator { op, got: (lhs, rhs) }, span))
                },
            }
        },
        Expr::Unary(ExprUnary { op, expr }) => {
            let r#type = short_curcuit!(compile_expr(*expr, scope, frame, globals)?);
            match (op, r#type) {
                (UnaryOperator::Negate(_), LanternType::Float) => {
                    inst!(frame.instructions; NEGF);
                    Ok(ControlFlow::Continue(LanternType::Float))
                },
                (UnaryOperator::Negate(_), LanternType::Integer) => {
                    inst!(frame.instructions; NEGI);
                    Ok(ControlFlow::Continue(LanternType::Integer))
                },
                (UnaryOperator::Not(_), LanternType::Bool) => {
                    inst!(frame.instructions; NOT);
                    Ok(ControlFlow::Continue(LanternType::Bool))
                },
                (op, got) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::UnaryOperator { op, got }, span))
                },
            }
        },
        Expr::Paren(ExprParen { expr, .. }) => compile_expr(*expr, scope, frame, globals),
        Expr::Block(ExprBlock { stmts, .. }) => {
            let block_scope = scope.child_block();
            short_curcuit!(compile_stmts(stmts, block_scope, frame, globals)?);
            inst!(frame.instructions; PUSHU 0);
            Ok(ControlFlow::Continue(LanternType::Null))
        },
        Expr::Array(ExprArray { elements, .. }) => {
            let len = elements.0.len();
            let mut inner = None;

            for expr in elements.0 {
                let span = expr.span().clone();
                inner = match (inner, compile_expr(expr, scope, frame, globals)?) {
                    (None, ControlFlow::Continue(r#type)) => Some(r#type),
                    (Some(r#type), ControlFlow::Continue(expr_type)) if r#type == expr_type => Some(r#type),
                    (Some(r#type), ControlFlow::Continue(expr_type)) => return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: r#type, got: expr_type }, span)),
                    (_, ControlFlow::Break(_)) => return Ok(ControlFlow::Break(())),
                }
            }

            inst! { frame.instructions;
                [PUSHU 1]
                [ALLOC_ARR len]
            }
            match inner {
                Some(inner) => Ok(ControlFlow::Continue(LanternType::Array(Box::new(inner)))),
                // TODO: type hint
                None => Ok(ControlFlow::Continue(LanternType::Array(Box::new(LanternType::Null)))),
            }
        },
        Expr::Index(ExprIndex { expr, index, .. }) => {
            let expr_span = expr.span().clone();
            let r#type = short_curcuit!(compile_expr(*expr, scope, frame, globals)?);
            let inner = match r#type {
                LanternType::Array(inner) => *inner,
                LanternType::String => LanternType::Integer,
                _ => return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Array(Box::new(LanternType::Null)), got: r#type }, expr_span)),
            };
            let index_span = index.span().clone();
            let index_type = short_curcuit!(compile_expr(*index, scope, frame, globals)?);
            if index_type != LanternType::Integer {
                return Err(CompilerError::new(CompilerErrorKind::TypeError { expected: LanternType::Integer, got: index_type }, index_span));
            }

            inst!(frame.instructions; INDEX);
            Ok(ControlFlow::Continue(inner))
        },
        Expr::Identifier(ident) => {
            let span = ident.1.clone();
            match scope.variable(&ident.0) {
                Some(var) => {
                    let local_index = frame.find_local(&ident.0).expect("local var exists");
                    inst!(frame.instructions; LOAD_LOCAL local_index);
                    Ok(ControlFlow::Continue(var.r#type))
                },
                None => {
                    let fun = scope.function(&ident.0)
                        .ok_or(CompilerError::new(CompilerErrorKind::UnknownVariable(ident), span))?;
                    inst!(frame.instructions; PUSHU fun.index as u64);
                    Ok(ControlFlow::Continue(LanternType::Function { args: fun.args.iter().map(|(_, r#type)| r#type.clone()).collect(), ret: Box::new(fun.ret.clone()) }))
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

