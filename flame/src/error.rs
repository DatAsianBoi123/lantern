#[derive(thiserror::Error, Debug, Clone, Copy, PartialEq, Eq)]
#[error("index out of bounds")]
pub struct IndexOutOfBoundsErr;

