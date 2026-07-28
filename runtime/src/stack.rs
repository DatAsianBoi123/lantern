use std::{fmt::{Display, Formatter}, mem::MaybeUninit, ops::{Index, IndexMut}};

use crate::{Slot, error::{AccessUndefinedError, StackOverflowError, StackUnderflowError}};

#[derive(Debug, Clone)]
pub struct LanternStack {
    inner: Box<[MaybeUninit<Slot>]>,
    top: usize,
}

impl Display for LanternStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, slot) in self.iter().enumerate() {
            write!(f, "{slot} ")?;
            if i % 2 == 1 {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl AsRef<[Slot]> for LanternStack {
    fn as_ref(&self) -> &[Slot] {
        // SAFETY: stack contains all valid Slots
        unsafe { self.inner[..self.top].assume_init_ref() }
    }
}

impl<'a> IntoIterator for &'a LanternStack {
    type IntoIter = Iter<'a>;
    type Item = Slot;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut LanternStack {
    type IntoIter = IterMut<'a>;
    type Item = &'a mut Slot;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl Index<usize> for LanternStack {
    type Output = Slot;

    fn index(&self, index: usize) -> &Self::Output {
        if let Ok(slot) = self.read(index) {
            slot
        } else {
            panic!("index out of bounds");
        }
    }
}

impl IndexMut<usize> for LanternStack {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        if let Ok(slot) = self.read_mut(index) {
            slot
        } else {
            panic!("index out of bounds");
        }
    }
}

impl LanternStack {
    pub fn new(len: usize) -> Self {
        Self {
            inner: Box::new_zeroed_slice(len),
            top: 0,
        }
    }

    pub fn top(&self) -> usize {
        self.top
    }

    pub fn top_mut(&mut self) -> &mut usize {
        &mut self.top
    }

    pub fn read(&self, addr: usize) -> Result<&Slot, AccessUndefinedError> {
        if addr > self.top { return Err(AccessUndefinedError); };
        unsafe { Ok(self.inner.get_unchecked(addr).assume_init_ref()) }
    }

    pub fn read_mut(&mut self, addr: usize) -> Result<&mut Slot, AccessUndefinedError> {
        if addr > self.top { return Err(AccessUndefinedError); };
        unsafe { Ok(self.inner.get_unchecked_mut(addr).assume_init_mut()) }
    }

    pub fn peek(&self) -> Result<Slot, StackUnderflowError> {
        if self.top == 0 { return Err(StackUnderflowError); };
        unsafe { Ok(self.inner.get_unchecked(self.top - 1).assume_init()) }
    }

    pub fn push_ref(&mut self, ptr: *const u8) -> Result<(), StackOverflowError> {
        self.push_slot(Slot::new_ref(ptr))
    }

    pub fn push_primitive<T>(&mut self, primitive: T) -> Result<(), StackOverflowError> {
        self.push_slot(Slot::new_primitive(primitive))
    }

    pub fn push_slot(&mut self, slot: Slot) -> Result<(), StackOverflowError> {
        // Slot does not have a destructor, so this will not cause leaks
        self.push()?.write(slot);
        Ok(())
    }

    fn push(&mut self) -> Result<&mut MaybeUninit<Slot>, StackOverflowError> {
        let slot = self.inner.get_mut(self.top).ok_or(StackOverflowError)?;
        self.top += 1;
        Ok(slot)
    }

    pub fn pop(&mut self) -> Result<Slot, StackUnderflowError> {
        self.top = self.top.checked_sub(1).ok_or(StackUnderflowError)?;
        unsafe { Ok(self.inner[self.top].assume_init()) }
    }

    pub fn pop_slice(&mut self, size: usize) -> Result<&[Slot], StackUnderflowError> {
        self.top = self.top.checked_sub(size).ok_or(StackUnderflowError)?;
        unsafe { Ok(self.inner[self.top..self.top + size].assume_init_ref()) }
    }

    pub fn iter(&self) -> Iter<'_> {
        Iter { inner: self.inner[0..self.top].iter() }
    }

    pub fn iter_mut(&mut self) -> IterMut<'_> {
        IterMut { inner: self.inner[0..self.top].iter_mut() }
    }
}

#[derive(Debug, Clone)]
pub struct Iter<'a> {
    inner: std::slice::Iter<'a, MaybeUninit<Slot>>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = Slot;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|slot| unsafe { slot.assume_init() })
    }
}

#[derive(Debug)]
pub struct IterMut<'a> {
    inner: std::slice::IterMut<'a, MaybeUninit<Slot>>,
}

impl<'a> Iterator for IterMut<'a> {
    type Item = &'a mut Slot;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|slot| unsafe { slot.assume_init_mut() })
    }
}

