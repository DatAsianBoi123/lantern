use std::fmt::{Display, Formatter};

use crate::{Slot, error::{AccessUndefinedError, StackOverflowError, StackUnderflowError}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStack {
    inner: [Slot; 1024],
    len: usize,
}

impl<'a> IntoIterator for &'a LanternStack {
    type IntoIter = std::slice::Iter<'a, Slot>;
    type Item = &'a Slot;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut LanternStack {
    type IntoIter = std::slice::IterMut<'a, Slot>;
    type Item = &'a mut Slot;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl Display for LanternStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, slot) in self.inner[0..self.len].iter().enumerate() {
            write!(f, "{slot} ")?;
            if i % 2 == 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl Default for LanternStack {
    fn default() -> Self {
        Self::new()
    }
}

impl LanternStack {
    pub fn new() -> Self {
        Self {
            inner: [Default::default(); 1024],
            len: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Returns the length of the stack in slots.
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn read(&self, addr: usize) -> Result<Slot, AccessUndefinedError> {
        if addr > self.len { return Err(AccessUndefinedError); };

        Ok(self.inner[addr])
    }

    pub fn peek(&self) -> Result<Slot, StackUnderflowError> {
        if self.len == 0 { return Err(StackUnderflowError); };
        Ok(self.inner[self.len - 1])
    }

    pub fn push_ref(&mut self, ptr: *const u8) -> Result<(), StackOverflowError> {
        self.push()?.write_ref(ptr);
        Ok(())
    }

    pub fn push_primitive<T>(&mut self, primitive: T) -> Result<(), StackOverflowError> {
        self.push()?.write_primitive(primitive);
        Ok(())
    }

    pub fn push_slot(&mut self, slot: Slot) -> Result<(), StackOverflowError> {
        *self.push()? = slot;
        Ok(())
    }

    fn push(&mut self) -> Result<&mut Slot, StackOverflowError> {
        if self.len >= self.inner.len() { return Err(StackOverflowError); };

        let slot = &mut self.inner[self.len];
        self.len += 1;
        Ok(slot)
    }

    pub fn pop(&mut self) -> Result<Slot, StackUnderflowError> {
        if self.len == 0 { return Err(StackUnderflowError); };
        self.len -= 1;
        Ok(self.inner[self.len])
    }

    pub fn pop_slice(&mut self, size: usize) -> Result<&[Slot], StackUnderflowError> {
        if self.len < size { return Err(StackUnderflowError); };
        self.len -= size;
        Ok(&self.inner[self.len..self.len + size])
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Slot> {
        self.inner[0..self.len].iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Slot> {
        self.inner[0..self.len].iter_mut()
    }
}

