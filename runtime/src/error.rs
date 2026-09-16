use std::fmt::{Display, Formatter};

#[derive(thiserror::Error, Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stacktrace: Vec<(String, StacktraceLocation)>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message)?;
        self.stacktrace.iter().take(30).try_for_each(|(function, location)| {
            write!(f, "\n  at {function} ({location})")
        })?;
        if self.stacktrace.len() > 30 {
            write!(f, "\n  ...and {} more", self.stacktrace.len() - 30)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StacktraceLocation {
    Line(u32),
    Native,
}

impl Display for StacktraceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Line(line) => write!(f, "line {line}"),
            Self::Native => write!(f, "<native function>"),
        }
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

