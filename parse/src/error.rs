use std::{error::Error, fmt::Display};

#[macro_export]
macro_rules! diagnostic {
    ($span: expr => $expr: expr) => {
        $crate::error::Diagnostic::new($expr.into(), $span)
    };

    ($span: expr, $($tt: tt)*) => {
        $crate::error::Diagnostic::new(::anyhow::anyhow!($($tt)*).into(), $span)
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub col: usize,
    pub line: usize,
}

impl Span {
    pub fn new(col: usize, line: usize) -> Self {
        Self { col, line }
    }
}

#[derive(thiserror::Error, Default, Debug)]
pub struct Diagnostics {
    errors: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new(errors: Vec<Diagnostic>) -> Self {
        Self { errors }
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.errors.push(diagnostic);
    }

    pub fn extend(&mut self, diagnostics: Diagnostics) {
        self.errors.extend(diagnostics.errors);
    }
}

impl Display for Diagnostics {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg = self.errors.iter()
            .map(|err| format!("error: {err}"))
            .collect::<Vec<String>>()
            .join("\n");
        write!(f, "{msg}")?;
        Ok(())
    }
}

impl From<Diagnostic> for Diagnostics {
    fn from(value: Diagnostic) -> Self {
        Self { errors: vec![value] }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("{error} (line {}, col {})", span.line, span.col)]
pub struct Diagnostic {
    error: Box<dyn Error>,
    pub span: Span,
}

impl Diagnostic {
    pub fn new(error: Box<dyn Error>, span: Span) -> Self {
        Self { error, span }
    }
}

#[derive(thiserror::Error, Debug)]
#[error("unexpected end of input")]
pub struct EndOfInputError;

