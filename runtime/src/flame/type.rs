use std::fmt::{Display, Formatter};

use parse::{FunType, Type};

use crate::flame::error::{CompilerError, CompilerErrorKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LanternType {
    Number,
    Bool,
    String,
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
            Self::Number => f.write_str("number"),
            Self::Bool => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Array(inner) => write!(f, "[{inner}]"),
            Self::Function { args, ret, .. } => {
                write!(f, "fun({}) -> {}", args.iter().map(|r#type| r#type.to_string()).collect::<Vec<String>>().join(", "), ret)
            },
            Self::Null => f.write_str("null"),
        }
    }
}

impl LanternType {
    pub fn from_type(r#type: &Type) -> Result<Self, CompilerError> {
        match r#type {
            Type::Array(_, inner, _) => Ok(Self::Array(Box::new(Self::from_type(inner)?))),
            Type::Fun(FunType { args, ret, .. }) => {
                let args = args.0.iter().map(LanternType::from_type).collect::<Result<_, _>>()?;
                let ret = ret.as_ref()
                    .map(|(_, r#type)| LanternType::from_type(r#type))
                    .unwrap_or(Ok(LanternType::Null))?;
                Ok(LanternType::Function { args, ret: Box::new(ret) })
            },
            Type::Path(path) => {
                match path.last().0.as_str() {
                    "float" => Ok(Self::Number),
                    "bool" => Ok(Self::Bool),
                    "str" => Ok(Self::String),
                    "none" => Ok(Self::Null),
                    _ => {
                        let span = path.items.1.clone();
                        Err(CompilerError::new(CompilerErrorKind::UnknownType(r#type.clone()), span))
                    },
                }
            },
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Number => 8,
            Self::Bool => 1,
            Self::String => 8,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::Null => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Number => 8,
            Self::Bool => 1,
            Self::String => 8,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::Null => 8,
        }
    }
}

