use instruction::InstructionSet;
use parse::{ast::{expr::{BinaryOperator, Expression, FunCall, UnaryOperator}, FunArg, FunDefinition, ItemAnnotation, LanternFile, ParenGroup, Punctuated, Statement, ValDeclaration}, lex::{Ident, Literal, LiteralKind}};

use crate::{error::{CompilerError, TypeError}, stack::{LanternFunction, LanternFunctionKind, StackFrame}, r#type::LanternType};

pub mod instruction;
pub mod error;
pub mod r#type;
pub mod stack;

pub type Address = usize;

pub fn ignite(file: LanternFile) -> Result<InstructionSet, CompilerError> {
    gen_instructions(file.statements, StackFrame::new())
}

fn gen_instructions(statements: Vec<Statement>, mut stack_frame: StackFrame) -> Result<InstructionSet, CompilerError> {
    let mut instructions = InstructionSet::new();
    inst!(instructions; STACK_PUSH);

    'outer: for statement in statements {
        match statement {
            Statement::ValDeclaration(ValDeclaration { init, .. }) => {
                compile_expr(init, &mut stack_frame, &mut instructions)?;
            },
            Statement::UsingStatement(_) => {},
            Statement::Expression(expr) => {
                compile_expr(expr, &mut stack_frame, &mut instructions)?;
            },
            Statement::FunDefinition(FunDefinition { annotations, native, ident, args, body, .. }) => {
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

                let mut fun_stack_frame = stack_frame.child();

                let args = args.0.items
                    .into_iter()
                    .enumerate()
                    .map(|(i, FunArg { ident, mut r#type, .. })| {
                        fun_stack_frame.insert_variable(ident.name, i);

                        let last = r#type.items.items.pop().expect("path has at least 1 segment").name;
                        match last.as_str() {
                            "float" => Ok(LanternType::Number),
                            "bool" => Ok(LanternType::Bool),
                            "str" => Ok(LanternType::String),
                            _ => Err(CompilerError::UnknownType(last)),
                        }
                    })
                    .collect::<Result<Vec<LanternType>, CompilerError>>()?;

                let fun_instructions = gen_instructions(body.0, fun_stack_frame)?;

                let kind = LanternFunctionKind::Custom(fun_instructions);
                stack_frame.insert_function(ident.name, LanternFunction::new(args, kind));
            },
        }
    }

    inst!(instructions; STACK_POP);
    Ok(instructions)
}

fn compile_expr(expression: Expression, stack_frame: &mut StackFrame, instructions: &mut InstructionSet) -> Result<LanternType, CompilerError> {
    match expression {
        Expression::Literal(Literal { kind: LiteralKind::Number(number) }) => {
            inst!(instructions; PUSHF number);
            Ok(LanternType::Number)
        },
        Expression::Literal(Literal { kind: LiteralKind::Boolean(bool) }) => {
            inst!(instructions; PUSHB bool as u8);
            Ok(LanternType::Bool)
        },
        Expression::Literal(Literal { kind: LiteralKind::String(string) }) => {
            let len = string.len();
            for byte in string.into_bytes() {
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
        Expression::FunCall(FunCall { ident, args: ParenGroup(Punctuated { items: call_args, .. }) }) => {
            if let Some(LanternFunction { args, kind }) = stack_frame.function(&ident.name).cloned() {
                if call_args.len() != args.len() {
                    return Err(CompilerError::MismatchedFunctionArgs { ident, expects: args.len(), got: call_args.len() });
                }

                for (expr, r#type) in call_args.into_iter().zip(args) {
                    let expr_type = compile_expr(expr, stack_frame, instructions)?;
                    if expr_type != r#type {
                        return Err(CompilerError::TypeError(TypeError::FunctionArgs { ident, expected: r#type, got: expr_type }));
                    }
                }

                match kind {
                    LanternFunctionKind::Native(id) => inst!(instructions; INV_NATIVE id),
                    LanternFunctionKind::Custom(mut fun_instructions) => instructions.inner.append(&mut fun_instructions.inner),
                }

                // TODO: function returns
                Ok(LanternType::Null)
            } else {
                Err(CompilerError::UnknownFunction(ident))
            }
        },
        Expression::BinaryAdd(lhs, rhs) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, LanternType::Number) => {
                    inst!(instructions; ADDF);
                    Ok(LanternType::Number)
                },
                (lhs, rhs) => Err(CompilerError::TypeError(TypeError::BinaryOperator { op: BinaryOperator::Add, got: (lhs, rhs) })),
            }
        },
        Expression::BinarySub(lhs, rhs) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, LanternType::Number) => {
                    inst!(instructions; SUBF);
                    Ok(LanternType::Number)
                },
                (lhs, rhs) => Err(CompilerError::TypeError(TypeError::BinaryOperator { op: BinaryOperator::Sub, got: (lhs, rhs) })),
            }
        },
        Expression::BinaryMult(lhs, rhs) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, LanternType::Number) => {
                    inst!(instructions; MULTF);
                    Ok(LanternType::Number)
                },
                (lhs, rhs) => Err(CompilerError::TypeError(TypeError::BinaryOperator { op: BinaryOperator::Mult, got: (lhs, rhs) })),
            }
        },
        Expression::BinaryDiv(lhs, rhs) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, LanternType::Number) => {
                    inst!(instructions; DIVF);
                    Ok(LanternType::Number)
                },
                (lhs, rhs) => Err(CompilerError::TypeError(TypeError::BinaryOperator { op: BinaryOperator::Div, got: (lhs, rhs) })),
            }
        },
        Expression::BinaryMod(lhs, rhs) => {
            match (compile_expr(*lhs, stack_frame, instructions)?, compile_expr(*rhs, stack_frame, instructions)?) {
                (LanternType::Number, LanternType::Number) => {
                    inst!(instructions; MODF);
                    Ok(LanternType::Number)
                },
                (lhs, rhs) => Err(CompilerError::TypeError(TypeError::BinaryOperator { op: BinaryOperator::Mod, got: (lhs, rhs) })),
            }
        },
        Expression::UnaryNegate(expr) => {
            match compile_expr(*expr, stack_frame, instructions)? {
                LanternType::Number => {
                    inst!(instructions; NEGF);
                    Ok(LanternType::Number)
                },
                got => Err(CompilerError::TypeError(TypeError::UnaryOperator { op: UnaryOperator::Negate, got })),
            }
        },
        Expression::UnaryNot(expr) => {
            match compile_expr(*expr, stack_frame, instructions)? {
                LanternType::Bool => {
                    inst!(instructions; NOT);
                    Ok(LanternType::Bool)
                },
                got => Err(CompilerError::TypeError(TypeError::UnaryOperator { op: UnaryOperator::Not, got })),
            }
        },
        _ => todo!(),
    }
}

