pub enum Instruction {
    Pushu8(u8),
    Pushf64(f64),
    InvokeNative(String),
    Pop(usize),
    Copy(Address, usize, Address),
}

pub type Address = usize;

