#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CompilerError {
    #[error("Unknown function {0}")]
    UnknownFunction(String),
}

