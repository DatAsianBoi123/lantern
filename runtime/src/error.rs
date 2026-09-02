use std::fmt::{Display, Formatter};

#[derive(thiserror::Error, Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stacktrace: Vec<(String, u32)>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message)?;
        for element in &self.stacktrace {
            write!(f, "\n  at {} (line {})", element.0, element.1)?;
        }
        Ok(())
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[error("{0}")]
pub struct UserError(pub String);

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[error("index {0} is out of bounds for len {1}")]
pub struct OutOfBoundsError(pub i64, pub usize);

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Stack overflow")]
pub struct StackOverflowError;

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Stack underflow")]
pub struct StackUnderflowError;

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Attempted to access undefined data")]
pub struct AccessUndefinedError;

