use error::IndexOutOfBoundsErr;
use instruction::{Instruction, InstructionSet};
use parse::{ast::{expr::Expression, LanternFile, Statement, ValDeclaration}, lex::{Literal, LiteralKind}};

pub mod instruction;
pub mod error;

pub type Address = usize;

pub fn ignite<const S: usize>(file: LanternFile) -> Result<InstructionSet<S>, IndexOutOfBoundsErr> {
    let mut instructions = InstructionSet::new();

    for statement in file.statements {
        match statement {
            Statement::ValDeclaration(ValDeclaration { init, .. }) => {
                compile_expr(init, &mut instructions)?;
            },
            Statement::UsingStatement(_) => {},
            _ => todo!(),
        }
    }

    Ok(instructions)
}

fn compile_expr<const S: usize>(expression: Expression, instructions: &mut InstructionSet<S>) -> Result<(), IndexOutOfBoundsErr> {
    match expression {
        Expression::Literal(Literal { kind: LiteralKind::Number(number) }) => instructions.push(Instruction::Pushf64(number))?,
        Expression::Literal(Literal { kind: LiteralKind::Boolean(bool) }) => instructions.push(Instruction::Pushu8(bool as u8))?,
        Expression::Literal(Literal { kind: LiteralKind::String(string) }) => {
            let len = string.len();
            for byte in string.into_bytes() {
                instructions.push(Instruction::Pushu8(byte))?;
            };
            instructions.push(Instruction::Pushusize(1))?;
            instructions.push(Instruction::Pushusize(len))?;
            instructions.push(Instruction::Alloc)?;
            instructions.push(Instruction::Pushusize(len))?;
            instructions.push(Instruction::Write)?;
            instructions.push(Instruction::Dealloc)?;
        },
        Expression::BinaryAdd(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            instructions.push(Instruction::Addf)?;
        },
        Expression::BinarySub(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            instructions.push(Instruction::Subf)?;
        },
        Expression::BinaryMult(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            instructions.push(Instruction::Multf)?;
        },
        Expression::BinaryDiv(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            instructions.push(Instruction::Divf)?;
        },
        Expression::BinaryMod(lhs, rhs) => {
            compile_expr(*lhs, instructions)?;
            compile_expr(*rhs, instructions)?;
            instructions.push(Instruction::Modf)?;
        },
        Expression::UnaryNegate(expr) => {
            compile_expr(*expr, instructions)?;
            instructions.push(Instruction::Negf)?;
        },
        Expression::UnaryNot(expr) => {
            compile_expr(*expr, instructions)?;
            instructions.push(Instruction::Not)?;
        },
        _ => todo!(),
    };
    Ok(())
}

