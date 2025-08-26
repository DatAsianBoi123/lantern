use std::{alloc::Layout, fmt::{Display, Formatter}, mem, slice};

use error::{AccessUndefinedError, RuntimeError, StackOverflowError, StackUnderflowError};
use flame::{instruction::{Instruction, InstructionSet}, Address};

macro_rules! args {
    (@args ( $ty: ty ) in $stack: expr) => {{
        let total_size = std::mem::size_of::<$ty>();
        (unsafe { *($stack.read($stack.ptr - total_size)? as *const $ty) }, total_size)
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
                    let arg = unsafe { *($stack.read(args_start + current_arg * std::mem::size_of::<$ty>())? as *const $ty) };
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

#[repr(C)]
struct HeapMetadata {
    size: usize,
    alignment: usize,
}

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
                Instruction::Pop(len) => { self.stack.pop(len)?; },
                Instruction::Copy(from, len, to) => { self.stack.copy(from, len, to)?; },
                Instruction::Addf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs + rhs),
                Instruction::Subf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs - rhs),
                Instruction::Multf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs * rhs),
                Instruction::Divf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs / rhs),
                Instruction::Modf => args!((f64, f64) in self.stack, (lhs, rhs) => lhs % rhs),
                Instruction::Negf => args!((f64) in self.stack, f64 => -f64),
                Instruction::Not => args!((bool) in self.stack, bool => !bool),
                Instruction::Alloc => args!((usize, usize) in self.stack, (alignment, size) => {
                    let metadata_size = mem::size_of::<HeapMetadata>();
                    let total_size = size + metadata_size + alignment;
                    unsafe {
                        let layout = Layout::from_size_align_unchecked(total_size, alignment.max(mem::align_of::<HeapMetadata>()));
                        let ptr = std::alloc::alloc(layout);
                        let data_ptr = align_up(ptr.byte_add(metadata_size), alignment);
                        let meta_ptr = data_ptr.byte_sub(metadata_size) as *mut HeapMetadata;
                        meta_ptr.write(HeapMetadata { size, alignment });

                        data_ptr
                    }
                }),
                Instruction::Dealloc => args!((usize) in self.stack, data_ptr => {
                    unsafe {
                        let meta_ptr = (data_ptr - mem::size_of::<HeapMetadata>()) as *mut HeapMetadata;
                        let meta = &*meta_ptr;

                        let total_size = meta.size + meta.alignment + mem::size_of::<HeapMetadata>();
                        let layout = Layout::from_size_align_unchecked(total_size, meta.alignment.max(mem::align_of::<HeapMetadata>()));

                        let ptr = meta_ptr.byte_sub(meta_ptr as usize % layout.align()) as *mut u8;
                        std::alloc::dealloc(ptr, layout);
                    };
                }),
                Instruction::Write => {
                    unsafe {
                        let size = *(self.stack.read(self.stack.ptr - mem::size_of::<usize>())? as *const usize);
                        self.stack.pop(mem::size_of::<usize>())?;
                        let ptr = *(self.stack.read(self.stack.ptr - mem::size_of::<usize>())? as *const usize);
                        let ptr = ptr as *mut u8;
                        self.stack.pop(mem::size_of::<usize>())?;
                        let data = self.stack.read(self.stack.ptr - mem::size_of::<u8>() * size)?;
                        self.stack.pop(mem::size_of::<u8>() * size)?;

                        std::ptr::copy_nonoverlapping(data, ptr, size);

                        self.stack.push(ptr)?;
                    };
                },
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
        for (i, byte) in self.stack.iter().enumerate() {
            if i != 0 {
                if i % 16 == 0 { writeln!(f)?; }
                else if i % 8 == 0 { write!(f, " ")?; };
            };
            write!(f, "{byte:02X} ")?;
        }
        Ok(())
    }
}

impl<const S: usize> Stack<S> {
    pub fn new() -> Self {
        Self { stack: [0; S], ptr: 0 }
    }

    pub fn read(&self, addr: Address) -> Result<*const u8, AccessUndefinedError> {
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

fn align_up(addr: *const u8, align: usize) -> *const u8 {
    let addr = addr as usize;
    ((addr + align - 1) & !(align - 1)) as *const u8
}

