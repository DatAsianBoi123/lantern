use instruction::InstructionSet;
use parse::{ast::{expr::{Expression, FunCall}, LanternFile, Statement, ValDeclaration}, lex::{Literal, LiteralKind}};

use crate::error::CompilerError;

pub mod instruction;
pub mod error;

pub type Address = usize;

pub fn ignite(file: LanternFile) -> Result<InstructionSet, CompilerError> {
    let mut instructions = InstructionSet::new();

    for statement in file.statements {
        match statement {
            Statement::ValDeclaration(ValDeclaration { init, .. }) => compile_expr(init, &mut instructions)?,
            Statement::UsingStatement(_) => {},
            Statement::Expression(expr) => compile_expr(expr, &mut instructions)?,
            _ => todo!(),
        }
    }

    Ok(instructions)
}

fn compile_expr(expression: Expression, instructions: &mut InstructionSet) -> Result<(), CompilerError> {
    match expression {
        Expression::Literal(Literal { kind: LiteralKind::Number(number) }) => inst!(instructions; PUSHF number),
        Expression::Literal(Literal { kind: LiteralKind::Boolean(bool) }) => inst!(instructions; PUSHB bool as u8),
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
        },
        Expression::FunCall(FunCall { ident, mut args }) => {
            if ident.name == "print" && args.0.items.len() == 1 {
                compile_expr(args.0.items.pop().unwrap(), instructions)?;
                inst!(instructions; INV_NATIVE 0x0001); // print
            } else {
                return Err(CompilerError::UnknownFunction(ident.name));
            }
        },
        Expression::BinaryAdd(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            inst!(instructions; ADDF);
        },
        Expression::BinarySub(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            inst!(instructions; SUBF);
        },
        Expression::BinaryMult(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            inst!(instructions; MULTF);
        },
        Expression::BinaryDiv(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            inst!(instructions; DIVF);
        },
        Expression::BinaryMod(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            inst!(instructions; MODF);
        },
        Expression::UnaryNegate(expr) => {
            compile_expr(*expr, instructions)?;
            inst!(instructions; NEGF);
        },
        Expression::UnaryNot(expr) => {
            compile_expr(*expr, instructions)?;
            inst!(instructions; NOT);
        },
        _ => todo!(),
    };
    Ok(())
}

