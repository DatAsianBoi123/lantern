use std::fmt::{Display, Formatter};

use diagnostic::{Diagnostic, error};
use parse::{FunType, Type, lex::TokenKind};

use crate::flame::{LanternItem, LanternStruct, scope::Scope};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LanternType {
    Integer,
    Float,
    Bool,
    String,
    Struct(LanternStruct),
    Array(Box<LanternType>),
    Function {
        args: Vec<LanternType>,
        ret: Box<LanternType>,
    },
    Null,
}

impl Display for LanternType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => f.write_str("int"),
            Self::Float => f.write_str("float"),
            Self::Bool => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Struct(_) => f.write_str("struct"),
            Self::Array(inner) => write!(f, "[{inner}]"),
            Self::Function { args, ret, .. } => {
                write!(f, "fun({}) -> {}", args.iter().map(|r#type| r#type.to_string()).collect::<Vec<String>>().join(", "), ret)
            },
            Self::Null => f.write_str("null"),
        }
    }
}

impl LanternType {
    pub fn from_type(r#type: &Type, scope: &Scope) -> Result<Self, Diagnostic> {
        match r#type {
            Type::Array(_, inner, _) => Ok(Self::Array(Box::new(Self::from_type(inner, scope)?))),
            Type::Fun(FunType { args, ret, .. }) => {
                let args = args.0.iter().map(|r#type| LanternType::from_type(r#type, scope)).collect::<Result<_, _>>()?;
                let ret = ret.as_ref()
                    .map(|(_, r#type)| LanternType::from_type(r#type, scope))
                    .unwrap_or(Ok(LanternType::Null))?;
                Ok(LanternType::Function { args, ret: Box::new(ret) })
            },
            Type::Path(path) => {
                match path.last().0.as_str() {
                    "int" => Ok(Self::Integer),
                    "float" => Ok(Self::Float),
                    "bool" => Ok(Self::Bool),
                    "str" => Ok(Self::String),
                    "none" => Ok(Self::Null),
                    last => {
                        let span = path.items.0[0].span();
                        match scope.item(last) {
                            Some(LanternItem::Struct(r#struct)) => Ok(Self::Struct(r#struct.clone())),
                            // FIXME: Err(CompilerError::new(CompilerErrorKind::UnknownType(r#type.clone()), span))
                            None => Err(error!(span => "unknown type `{type}`")),
                        }
                    },
                }
            },
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Integer | Self::Float | Self::Bool | Self::Function { .. })
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Integer => 8,
            Self::Float => 8,
            Self::Bool => 1,
            Self::String => 8,
            Self::Struct(_) => 8,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::Null => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Integer => 8,
            Self::Float => 8,
            Self::Bool => 1,
            Self::String => 8,
            Self::Struct(_) => 8,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::Null => 8,
        }
    }
}

