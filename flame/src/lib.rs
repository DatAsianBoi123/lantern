#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Pushu8(u8),
    Pushf64(f64),
    InvokeNative(&'static str),
    Pop(usize),
    Copy(Address, usize, Address),
    NULL,
}

pub type Address = usize;

