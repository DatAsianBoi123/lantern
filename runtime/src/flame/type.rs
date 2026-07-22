use std::fmt::{Display, Formatter};

use diagnostic::{Diagnostic, error};
use parse::{FunType, Type, lex::TokenKind};

use crate::flame::{LanternItem, LanternPrimitive, LanternStruct, native, scope::{ItemIdentifier, Scope}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LanternType {
    Struct(usize),
    Primitive(&'static LanternPrimitive),
    Array(Box<LanternType>),
    Function {
        is_method: bool,
        args: Vec<LanternType>,
        ret: Box<LanternType>,
    },
    ItemStatic(ItemIdentifier),
    Null,
}

impl Display for LanternType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Struct(_) => f.write_str("struct"),
            Self::Primitive(_) => f.write_str("primitive"),
            Self::Array(inner) => write!(f, "[{inner}]"),
            Self::Function { args, ret, .. } => {
                write!(f, "fun({}) -> {}", args.iter().map(|r#type| r#type.to_string()).collect::<Vec<String>>().join(", "), ret)
            },
            Self::ItemStatic(ItemIdentifier::Struct(id)) => write!(f, "item(struct:{id})"),
            Self::ItemStatic(ItemIdentifier::Primitive(id)) => write!(f, "item(primitive:{id})"),
            Self::Null => f.write_str("null"),
        }
    }
}

impl LanternType {
    pub fn from_type(r#type: &Type, scope: &Scope) -> Result<Self, Diagnostic> {
        match r#type {
            Type::Array(_, inner, _) => Ok(Self::Array(Box::new(Self::from_type(inner, scope)?))),
            Type::Fun(FunType { args, ret, .. }) => {
                let args = args.iter().map(|r#type| LanternType::from_type(r#type, scope)).collect::<Result<_, _>>()?;
                let ret = ret.as_ref()
                    .map(|(_, r#type)| LanternType::from_type(r#type, scope))
                    .unwrap_or(Ok(LanternType::Null))?;
                Ok(LanternType::Function { is_method: false, args, ret: Box::new(ret) })
            },
            Type::Path(path) => {
                let span = path.items[0].span();
                match scope.item(&path.last().0) {
                    Some(LanternItem::Struct(LanternStruct { id, .. })) => Ok(Self::Struct(*id)),
                    Some(LanternItem::Primitive(primitive)) => Ok(Self::Primitive(primitive)),
                    None => Err(error!(span => "unknown type `{type}`")),
                }
            },
        }
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_) | Self::Function { .. })
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Struct(_) | Self::Array(..))
    }

    pub fn is_bool(&self) -> bool {
        *self == Self::Primitive(&native::BOOL_PRIMITIVE)
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Struct(_) => 8,
            Self::Primitive(LanternPrimitive { size, .. }) => *size,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::ItemStatic(_) => panic!("static types are unsized"),
            // null is a ptr
            Self::Null => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Struct(_) => 8,
            Self::Primitive(LanternPrimitive { align, .. }) => *align,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            Self::ItemStatic(_) => panic!("static types have no alignment"),
            // null is a ptr
            Self::Null => 8,
        }
    }
}

