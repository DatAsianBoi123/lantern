use std::error::Error;

macro_rules! runtime_err_from {
    ($($e: ident),* $(,)?) => {
        $(
        impl From<$e> for RuntimeError {
            fn from(error: $e) -> Self {
                Self(Box::new(error))
            }
        }
        )*
    };
}

#[derive(thiserror::Error, Debug)]
#[error("Lantern runtime crashed: {0}")]
pub struct RuntimeError(#[from] pub Box<dyn Error>);

runtime_err_from![CopyError, StackOverflowError, StackUnderflowError, AccessUndefinedError];

#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq)]
pub enum CopyError {
    #[error("The source and destination overlap")]
    Overlapping,

    #[error(transparent)]
    AccessUndefined(#[from] AccessUndefinedError),
}

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Stack overflow")]
pub struct StackOverflowError;

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Stack underflow")]
pub struct StackUnderflowError;

#[derive(thiserror::Error, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[error("Attempted to access undefined data")]
pub struct AccessUndefinedError;

