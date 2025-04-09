use crate::{error::IndexOutOfBoundsErr, Address};

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionSet<const S: usize> {
    inner: [Instruction; S],
    idx: usize,
}

impl<const S: usize> Default for InstructionSet<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const S: usize> InstructionSet<S> {
    pub fn new() -> Self {
        Self {
            inner: [const { Instruction::NULL }; S],
            idx: 0,
        }
    }

    pub fn push(&mut self, instruction: Instruction) -> Result<(), IndexOutOfBoundsErr> {
        if self.idx >= self.inner.len() { return Err(IndexOutOfBoundsErr); };
        self.inner[self.idx] = instruction;
        self.idx += 1;
        Ok(())
    }
}

impl<const S: usize> IntoIterator for InstructionSet<S> {
    type IntoIter = std::array::IntoIter<Instruction, S>;
    type Item = Instruction;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Pushu8(u8),
    Pushusize(usize),
    Pushf64(f64),
    PushHeap(usize, usize),
    Pop(usize),
    PopHeap(Address, usize, usize),
    Copy(Address, usize, Address),
    Addf,
    Subf,
    Multf,
    Divf,
    Modf,
    Negf,
    Not,
    InvokeNative(&'static str),
    NULL,
}

