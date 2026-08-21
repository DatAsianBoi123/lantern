use std::fmt::{Display, Formatter};

pub mod symbol;

#[macro_export]
macro_rules! error {
    (in $sink:expr; $span:expr => $($tt:tt)*) => {
        $sink.emit(error!($span => $($tt)*))
    };
    ($span:expr => $($tt:tt)*) => {
        ::diagnostic::Diagnostic::new_error(format!($($tt)*), $span)
    };
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticSink {
    emitted: Vec<Diagnostic>,
    fatal: bool,
}

impl DiagnosticSink {
    pub fn new() -> Self {
        Self { emitted: Vec::new(), fatal: false }
    }

    pub fn emit(&mut self, diagnostic: Diagnostic) {
        if matches!(diagnostic.level, DiagnosticLevel::Error) {
            self.fatal = true;
        }
        self.emitted.push(diagnostic);
    }

    pub fn emit_or<T>(&mut self, res: Result<T, Diagnostic>, def: T) -> T {
        match res {
            Ok(t) => t,
            Err(err) => {
                self.emit(err);
                def
            }
        }
    }

    pub fn fatal(&self) -> bool {
        self.fatal
    }

    pub fn into_emitted(self) -> Vec<Diagnostic> {
        self.emitted
    }
}

// TODO: DiagnosticMessage enum
#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
#[error("{level}: {message} ({}:{})", span.line(), span.col())]
pub struct Diagnostic {
    pub message: String,
    pub span: Span,
    pub level: DiagnosticLevel,
}

impl Diagnostic {
    pub fn new_error(message: String, span: Span) -> Self {
        Diagnostic { message, span, level: DiagnosticLevel::Error }
    }

    pub fn new_warning(message: String, span: Span) -> Self {
        Diagnostic { message, span, level: DiagnosticLevel::Warning }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DiagnosticLevel {
    Error,
    Warning,
}

impl Display for DiagnosticLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Error => write!(f, "error"),
            Self::Warning => write!(f, "warning"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    line: u32,
    col: u32,
}

impl Span {
    pub fn new(line: u32, col: u32) -> Self {
        Self { line, col }
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }
}

