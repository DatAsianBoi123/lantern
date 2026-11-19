use std::{fmt::{Display, Formatter}, path::PathBuf};

#[derive(thiserror::Error, Debug)]
pub struct RuntimeError {
    pub message: String,
    pub stacktrace: Vec<Trace>,
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {}", self.message)?;
        self.stacktrace.iter().take(30).try_for_each(|trace| write!(f, "\n  {trace}"))?;
        if self.stacktrace.len() > 30 {
            write!(f, "\n  ...and {} more", self.stacktrace.len() - 30)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Trace {
    pub module: PathBuf,
    pub function: String,
    pub loc: StacktraceLocation,
}

impl Display for Trace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "at {} ({}:{})", self.function, self.module.to_string_lossy(), self.loc)
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
            Self::Line(line) => write!(f, "{line}"),
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

