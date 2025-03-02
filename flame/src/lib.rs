use error::IndexOutOfBoundsErr;
use instruction::{Instruction, InstructionSet};
use parse::{ast::{Expression, LanternFile, Statement, ValDeclaration}, lex::LiteralKind};

pub mod instruction;
pub mod error;

pub type Address = usize;

pub fn ignite<const S: usize>(file: LanternFile) -> Result<InstructionSet<S>, IndexOutOfBoundsErr> {
    let mut instructions = InstructionSet::new();

    for statement in file.statements {
        match statement {
            Statement::ValDeclaration(ValDeclaration { init: Expression::Literal(literal), .. }) => {
                match literal.kind {
                    LiteralKind::Number(number) => instructions.push(Instruction::Pushf64(number))?,
                    LiteralKind::String(string) => {
                        for byte in string.into_bytes() {
                            // TODO: string in heap
                            instructions.push(Instruction::Pushu8(byte))?;
                        }
                    },
                }
            },
            _ => todo!(),
        }
    }

    Ok(instructions)
}

