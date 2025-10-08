use parse::{ast::expr::{BinaryOperator, UnaryOperator}, lex::Ident};

use crate::r#type::LanternType;

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq)]
pub enum CompilerError {
    #[error("Unknown function '{}'", _0.name)]
    UnknownFunction(Ident),

    #[error("Unknown type '{0}'")]
    UnknownType(String),

    #[error("Function '{0}' is missing an @NativeID annotation")]
    BadNative(String),

    #[error("Function '{}' expects {expects} args, but got {got} instead", ident.name)]
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
    #[error("Expected arg of type {expected}, but got {got} instead for function {}", ident.name)]
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

