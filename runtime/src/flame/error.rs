use parse::{Ident, Type, error::Span, expr::{BinaryOperator, UnaryOperator}};

use crate::flame::r#type::LanternType;

#[derive(thiserror::Error, Debug, Clone)]
#[error("{kind} (line {}, col {})", span.line, span.col)]
pub struct CompilerError {
    pub span: Span,
    pub kind: Box<CompilerErrorKind>,
}

impl CompilerError {
    pub fn new(kind: CompilerErrorKind, span: Span) -> Self {
        Self { span, kind: Box::new(kind) }
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CompilerErrorKind {
    #[error("Unknown variable '{0}'")]
    UnknownVariable(Ident),

    #[error("Unknown type '{0}'")]
    UnknownType(Type),

    #[error("Unknown native function '{0}'")]
    UnknownNative(Ident),

    #[error("Item '{0}' already declared")]
    ItemAlreadyDeclared(Ident),

    #[error("Function expects {expects} args, but got {got} instead")]
    MismatchedFunctionArgs {
        expects: usize,
        got: usize,
    },

    #[error("'return' not allowed here")]
    BadReturn,

    #[error("'break' can only be used in loops")]
    BadBreak,

    #[error("Expected type {expected}, but got {got} instead")]
    TypeError {
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

