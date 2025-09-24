#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CompilerError {
    #[error("Unknown function {0}")]
    UnknownFunction(String),

    #[error("Instruction set overflowed")]
    InstructionOverflow(#[from] IndexOutOfBoundsErr),
}

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq)]
#[error("index out of bounds")]
pub struct IndexOutOfBoundsErr;

