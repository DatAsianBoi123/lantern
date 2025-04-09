use std::{alloc::{handle_alloc_error, GlobalAlloc, Layout, System}, fmt::{Display, Formatter}, mem, slice};

use error::{AccessUndefinedError, RuntimeError, StackOverflowError, StackUnderflowError};
use flame::{instruction::{Instruction, InstructionSet}, Address};

macro_rules! args {
    (@args ( $ty: ty ) in $stack: expr) => {{
        let total_size = std::mem::size_of::<$ty>();
        (unsafe { *($stack.access($stack.ptr - total_size)? as *const $ty) }, total_size)
    }};
    (@args ( $($ty: ty),+ $(,)? ) in $stack: expr) => {{
        let mut total_size = 0;
        $(
            total_size += std::mem::size_of::<$ty>();
        )+
        let args_start = $stack.ptr - total_size;
        let mut current_arg = 0;
        #[allow(unused_assignments)]
        let args = (
            $(
                {
                    let arg = unsafe { *($stack.access(args_start + current_arg * std::mem::size_of::<$ty>())? as *const $ty) };
                    current_arg += 1;
                    arg
                }
            ),+
        );
        (args, total_size)
    }};

    ( ( $($ty: ty),+ $(,)? ) in $stack: expr, $pat: pat => $ret: expr) => {{
        let (args, total_size) = args!(@args ($($ty),+) in $stack);

        let $pat = args;
        $stack.pop(total_size)?;
        $stack.push($ret)?;
    }};
}

pub mod error;

#[derive(Debug, Clone, PartialEq)]
pub struct LanternRuntime<const S: usize, const T: usize> {
    stack: Stack<S>,
    text: InstructionSet<T>,
}

impl<const S: usize, const T: usize> LanternRuntime<S, T> {
    pub fn new(instructions: InstructionSet<T>) -> Self {
        Self { stack: Default::default(), text: instructions }
    }

    pub fn exec(mut self) -> Result<Stack<S>, RuntimeError> {
        for instruction in self.text {
            match instruction {
                Instruction::Pushu8(u8) => { self.stack.push(u8)?; },
                Instruction::Pushusize(usize) => { self.stack.push(usize)?; },
                Instruction::Pushf64(f64) => { self.stack.push(f64)?; },
                Instruction::PushHeap(size, align) => {
                    if size == 0 { panic!("attempted to alloc 0 bytes"); };
                    let layout = unsafe { Layout::from_size_align_unchecked(size, align) };
                    let heap_ptr = unsafe { System.alloc(layout) };
                    if heap_ptr.is_null() { handle_alloc_error(layout); };
                    unsafe { std::ptr::copy_nonoverlapping(self.stack.access(self.stack.ptr - size)?, heap_ptr, size); };
                    println!("allocated {layout:?} at {heap_ptr:?}, is {:02X?}", unsafe { slice::from_raw_parts(heap_ptr, size) });
                    self.stack.pop(size)?;
                    self.stack.push(heap_ptr)?;
                },
                Instruction::Pop(len) => { self.stack.pop(len)?; },
                Instruction::PopHeap(address, size, align) => {
                    let layout = unsafe { Layout::from_size_align_unchecked(size, align) };
                    unsafe { System.dealloc(address as *mut u8, layout); };
                    println!("deallocated {address:02X} ({layout:?})");
                },
                Instruction::Copy(from, len, to) => { self.stack.copy(from, len, to)?; },
                Instruction::Addf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs + rhs),
                Instruction::Subf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs - rhs),
                Instruction::Multf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs * rhs),
                Instruction::Divf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs / rhs),
                Instruction::Modf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs % rhs),
                Instruction::Negf => args!((f64) in self.stack, f64 => -f64),
                Instruction::Not => args!((bool) in self.stack, bool => !bool),
                Instruction::InvokeNative(name) => {
                    match name {
                        "print_f64" => args!((f64) in self.stack, f64 => println!("{f64}")),
                        _ => panic!("unknown native function {name}"),
                    }
                },
                Instruction::NULL => break,
            };
        };

        Ok(self.stack)
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

impl<const S: usize> Display for Stack<S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for byte in self.stack {
            write!(f, "{byte:02X} ")?;
        }
        Ok(())
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

