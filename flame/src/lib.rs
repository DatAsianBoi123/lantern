use instruction::InstructionSet;
use parse::{Boolean, FunArg, Item, ItemFun, LanternFile, Literal, Stmt, ValDeclaration, expr::{BinaryOperator, Expr, ExprBinary, ExprBlock, ExprFunCall, ExprParen, ExprUnary, UnaryOperator}};

use crate::{error::{CompilerError, CompilerErrorKind, TypeError}, stack::{LanternFunction, LanternFunctionKind, StackFrame}, r#type::LanternType};

pub mod instruction;
pub mod error;
pub mod r#type;
pub mod stack;

pub type Address = usize;

pub fn ignite(file: LanternFile) -> Result<InstructionSet, CompilerError> {
    gen_instructions(file.stmts, StackFrame::new())
}

fn gen_instructions(statements: Vec<Stmt>, mut stack_frame: StackFrame) -> Result<InstructionSet, CompilerError> {
    let mut instructions = InstructionSet::new();
    inst!(instructions; STACK_PUSH);

    'outer: for statement in statements {
        match statement {
            Stmt::ValDeclaration(ValDeclaration { init: None, .. }) => {
                todo!()
            },
            Stmt::ValDeclaration(ValDeclaration { init: Some((_, init)), .. }) => {
                compile_expr(init, &mut stack_frame, &mut instructions)?;
            },
            Stmt::Expr(expr, _) => {
                compile_expr(expr, &mut stack_frame, &mut instructions)?;
            },
            Stmt::Item(Item::Using(_)) => {},
            Stmt::Item(Item::Fun(ItemFun { ident, args, block, .. })) => {
                /*
                if native.is_some() {
                    for ItemAnnotation { ident: Ident { name }, args: ParenGroup(Punctuated { items, .. }), .. } in annotations {
                        if name == "NativeID" && items.len() == 1 {
                            if let Expression::Literal(Literal { kind: LiteralKind::Number(num) }) = items[0] {
                                let kind = LanternFunctionKind::Native(num as u32);
                                let args = args.0.items
                                    .into_iter()
                                    .map(|FunArg { mut r#type, .. }| {
                                        let last = r#type.items.items.pop().expect("path has at least 1 segment").name;
                                        match last.as_str() {
                                            "float" => Ok(LanternType::Number),
                                            "bool" => Ok(LanternType::Bool),
                                            "str" => Ok(LanternType::String),
                                            _ => Err(CompilerError::UnknownType(last)),
                                        }
                                    })
                                    .collect::<Result<Vec<LanternType>, CompilerError>>()?;
                                stack_frame.insert_function(ident.name, LanternFunction::new(args, kind));
                                continue 'outer;
                            }
                        }
                    }

                    return Err(CompilerError::BadNative(ident.name));
                }
                */

                let mut fun_stack_frame = stack_frame.child();

                let args = args.0
                    .into_iter()
                    .enumerate()
                    .map(|(i, FunArg { ident, r#type, .. })| {
                        fun_stack_frame.insert_variable(ident.0, i);

                        let last = r#type.into_last().0;
                        match last.as_str() {
                            "float" => Ok(LanternType::Number),
                            "bool" => Ok(LanternType::Bool),
                            "str" => Ok(LanternType::String),
                            _ => Err(CompilerError::new(CompilerErrorKind::UnknownType(last), ident.1)),
                        }
                    })
                    .collect::<Result<Vec<LanternType>, CompilerError>>()?;

                let fun_instructions = gen_instructions(block.stmts, fun_stack_frame)?;

                let kind = LanternFunctionKind::Custom(fun_instructions);
                stack_frame.insert_function(ident.0, LanternFunction::new(args, kind));
            },
        }
    }

    inst!(instructions; STACK_POP);
    Ok(instructions)
}

fn compile_expr(expression: Expr, stack_frame: &mut StackFrame, instructions: &mut InstructionSet) -> Result<LanternType, CompilerError> {
    match expression {
        Expr::Literal(Literal::Number(number)) => {
            inst!(instructions; PUSHF number.0);
            Ok(LanternType::Number)
        },
        Expr::Literal(Literal::Boolean(bool)) => {
            match bool {
                Boolean::True(_) => inst!(instructions; PUSHB 1),
                Boolean::False(_) => inst!(instructions; PUSHB 0),
            }
            Ok(LanternType::Bool)
        },
        Expr::Literal(Literal::String(string)) => {
            let len = string.0.len();
            for byte in string.0.into_bytes() {
                inst!(instructions; BPUSH byte);
            };
            inst! { instructions;
                [PUSHU 1]
                [PUSHU len]
                [ALLOC]
                [WRITE]
                [PUSHU len]
            };
            Ok(LanternType::String)
        },
        Expr::FunCall(ExprFunCall { ident, args, .. }) => {
            if let Some(LanternFunction { args: fun_args, kind }) = stack_frame.function(&ident.0).cloned() {
                if args.len() != fun_args.len() {
                    let span = ident.1.clone();
                    return Err(CompilerError::new(CompilerErrorKind::MismatchedFunctionArgs { ident, expects: fun_args.len(), got: args.len() }, span));
                }

                for (expr, r#type) in args.into_iter().zip(fun_args) {
                    let expr_type = compile_expr(expr, stack_frame, instructions)?;
                    if expr_type != r#type {
                        let span = ident.1.clone();
                        return Err(CompilerError::new(CompilerErrorKind::TypeError(TypeError::FunctionArgs { ident, expected: r#type, got: expr_type }), span));
                    }
                }

                match kind {
                    LanternFunctionKind::Native(id) => inst!(instructions; INV_NATIVE id),
                    LanternFunctionKind::Custom(mut fun_instructions) => instructions.inner.append(&mut fun_instructions.inner),
                }

                // TODO: function returns
                Ok(LanternType::Null)
            } else {
                let span = ident.1.clone();
                Err(CompilerError::new(CompilerErrorKind::UnknownFunction(ident), span))
            }
        },
        Expr::Binary(ExprBinary { lhs, op, rhs }) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, op, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, BinaryOperator::Add(_), LanternType::Number) => {
                    inst!(instructions; ADDF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Sub(_), LanternType::Number) => {
                    inst!(instructions; SUBF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Mult(_), LanternType::Number) => {
                    inst!(instructions; MULTF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Div(_), LanternType::Number) => {
                    inst!(instructions; DIVF);
                    Ok(LanternType::Number)
                },
                (LanternType::Number, BinaryOperator::Mod(_), LanternType::Number) => {
                    inst!(instructions; MODF);
                    Ok(LanternType::Number)
                },
                (lhs, op, rhs) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::TypeError(TypeError::BinaryOperator { op, got: (lhs, rhs) }), span))
                },
            }
        },
        Expr::Unary(ExprUnary { op, expr }) => {
            match (op, compile_expr(*expr, stack_frame, instructions)?) {
                (UnaryOperator::Negate(_), LanternType::Number) => {
                    inst!(instructions; NEGF);
                    Ok(LanternType::Number)
                },
                (UnaryOperator::Not(_), LanternType::Bool) => {
                    inst!(instructions; NOT);
                    Ok(LanternType::Bool)
                },
                (op, got) => {
                    let span = op.span().clone();
                    Err(CompilerError::new(CompilerErrorKind::TypeError(TypeError::UnaryOperator { op, got }), span))
                },
            }
        },
        Expr::Paren(ExprParen { expr, .. }) => compile_expr(*expr, stack_frame, instructions),
        Expr::Block(ExprBlock { stmts, .. }) => {
            // TODO: returns
            let stack_frame = stack_frame.child();
            gen_instructions(stmts, stack_frame)?;
            Ok(LanternType::Null)
        },
        Expr::Variable(_) => todo!(),
    }
}

