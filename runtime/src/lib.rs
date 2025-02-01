use std::{mem, slice};

use error::{AccessUndefinedError, RuntimeError, StackOverflowError, StackUnderflowError};
use flame::{Address, Instruction};

pub mod error;

#[derive(Debug, Clone, PartialEq)]
pub struct LanternRuntime<const S: usize, const T: usize> {
    stack: Stack<S>,
    text: [Instruction; T],
}

impl<const S: usize, const T: usize> LanternRuntime<S, T> {
    pub fn new(instructions: [Instruction; T]) -> Self {
        Self { stack: Default::default(), text: instructions }
    }

    pub fn exec(mut self) -> Result<(), RuntimeError> {
        for instruction in self.text {
            match instruction {
                Instruction::Pushu8(u8) => { self.stack.push(u8)?; },
                Instruction::Pushf64(f64) => { self.stack.push(f64)?; },
                Instruction::InvokeNative(name) => {
                    match name {
                        "add_f64" => {
                            let args_start = self.stack.ptr - 2 * mem::size_of::<f64>();
                            let left = self.stack.access(args_start)? as *const f64;
                            let right = self.stack.access(args_start + mem::size_of::<f64>())? as *const f64;
                            let res = unsafe { *left + *right };

                            self.stack.push(res)?;
                        },
                        "print_f64" => {
                            let f64 = self.stack.access(self.stack.ptr - mem::size_of::<f64>())? as *const f64;
                            let f64 = unsafe { *f64 };
                            println!("{f64}");
                        },
                        _ => panic!("unknown native function {name}"),
                    }
                },
                Instruction::Pop(len) => { self.stack.pop(len)?; },
                Instruction::Copy(from, len, to) => { self.stack.copy(from, len, to)?; },
                Instruction::NULL => break,
            };
        };

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack<const S: usize> {
    stack: [u8; S],
    ptr: Address,
}

impl<const S: usize> Default for Stack<S> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const S: usize> Stack<S> {
    pub fn new() -> Self {
        Self { stack: [0; S], ptr: 0 }
    }

    pub fn access(&self, addr: Address) -> Result<*const u8, AccessUndefinedError> {
        if addr > self.ptr { return Err(AccessUndefinedError); };

        Ok(&self.stack[addr] as *const u8)
    }

    pub fn push<T>(&mut self, item: T) -> Result<usize, StackOverflowError> {
        let size = mem::size_of::<T>();

        let before = self.ptr;
        self.ptr += size;

        if self.ptr >= self.stack.len() { return Err(StackOverflowError); };

        let bytes = unsafe { slice::from_raw_parts(&item as *const T as *const u8, size) };
        self.stack[before..self.ptr].copy_from_slice(bytes);

        Ok(size)
    }

    pub fn pop(&mut self, len: usize) -> Result<(), StackUnderflowError> {
        if len > self.ptr { return Err(StackUnderflowError); };
        self.ptr -= len;
        Ok(())
    }

    pub fn copy(&mut self, from: Address, len: usize, to: Address) -> Result<(), AccessUndefinedError> {
        if from + len > self.ptr { return Err(AccessUndefinedError); };
        if to + len > self.ptr { return Err(AccessUndefinedError); };

        self.stack.copy_within(from..from + len, to);

        Ok(())
    }
}

