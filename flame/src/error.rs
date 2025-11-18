use parse::{Ident, error::Span, expr::{BinaryOperator, UnaryOperator}};

use crate::r#type::LanternType;

#[derive(thiserror::Error, Debug, Clone, )]
#[error("{kind} (line {}, col {})", span.line, span.col)]
pub struct CompilerError {
    pub span: Span,
    pub kind: CompilerErrorKind,
}

impl CompilerError {
    pub fn new(kind: CompilerErrorKind, span: Span) -> Self {
        Self { span, kind }
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CompilerErrorKind {
    #[error("Unknown function '{}'", _0.0)]
    UnknownFunction(Ident),

    #[error("Unknown type '{0}'")]
    UnknownType(String),

    #[error("Function '{0}' is missing an @NativeID annotation")]
    BadNative(String),

    #[error("Function '{}' expects {expects} args, but got {got} instead", ident.0)]
    MismatchedFunctionArgs {
        ident: Ident,
        expects: usize,
        got: usize,
    },

    #[error("Type error: {0}")]
    TypeError(#[from] TypeError),
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    #[error("Expected arg of type {expected}, but got {got} instead for function {}", ident.0)]
    FunctionArgs {
        ident: Ident,
        expected: LanternType,
        got: LanternType,
    },

    #[error("Binary operator '{op}' cannot be applied to types {} and {}", got.0, got.1)]
    BinaryOperator {
        op: BinaryOperator,
        got: (LanternType, LanternType),
    },

    #[error("Unary operator '{op}' cannot be applied to type {got}")]
    UnaryOperator {
        op: UnaryOperator,
        got: LanternType,
    }
}

