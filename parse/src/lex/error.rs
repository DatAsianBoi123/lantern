use std::{error::Error, fmt::Display};

#[macro_export]
macro_rules! diagnostic {
    ($($tt: tt)*) => {
        Diagnostic::new(::anyhow::anyhow!($($tt)*).into())
    };
}

#[derive(thiserror::Error, Debug)]
pub struct Diagnostics {
    errors: Box<[Diagnostic]>
}

impl Diagnostics {
    pub fn new(errors: Vec<Diagnostic>) -> Self {
        Self { errors: errors.into_boxed_slice() }
    }

    pub fn from_slice<const S: usize>(errors: [Diagnostic; S]) -> Self {
        Self { errors: Box::new(errors) }
    }
}

impl Display for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for err in &self.errors {
            write!(f, "error: {err}")?;
        }
        Ok(())
    }
}

impl From<Diagnostic> for Diagnostics {
    fn from(value: Diagnostic) -> Self {
        Self { errors: Box::new([value]) }
    }
}

#[derive(thiserror::Error, Debug)]
#[error(transparent)]
pub struct Diagnostic {
    #[from]
    error: Box<dyn Error>
}

impl Diagnostic {
    pub fn new(error: Box<dyn Error>) -> Self {
        Self { error }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("invalid token {0}")]
pub struct InvalidTokenError<T: Display>(pub T);

