use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LanternType {
    Number,
    Bool,
    String,
    Null,
}

impl Display for LanternType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number => f.write_str("number"),
            Self::Bool => f.write_str("bool"),
            Self::String => f.write_str("string"),
            Self::Null => f.write_str("null"),
        }
    }
}

