use std::{alloc::Layout, fmt::{Display, Formatter}, mem, ops::Deref};

use error::{AccessUndefinedError, RuntimeError, StackOverflowError, StackUnderflowError};
use flame::{instruction::{Instruction, InstructionSet}, Address};

use crate::error::CopyError;

macro_rules! args {
    (@args ( $ty: ty ) in $stack: expr) => {{
        (unsafe { *($stack.read($stack.len - SLOT_SIZE)? as *const $ty) }, SLOT_SIZE)
    }};
    (@args ( $($ty: ty),+ $(,)? ) in $stack: expr) => {{
        let mut total_size = 0;
        $(
            let _ = std::mem::size_of::<$ty>(); // for repeat
            total_size += SLOT_SIZE;
        )+
        let args_start = $stack.len - total_size;
        let mut current_arg = 0;
        #[allow(unused_assignments)]
        let args = (
            $(
                {
                    let arg = unsafe { *($stack.read(args_start + current_arg * SLOT_SIZE)? as *const $ty) };
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

pub const SLOT_SIZE: usize = 8;

#[repr(C)]
struct HeapMetadata {
    size: usize,
    alignment: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternRuntime<const T: usize> {
    stack: Stack,
    text: InstructionSet<T>,
}

impl<const T: usize> LanternRuntime<T> {
    pub fn new(stack_size: usize, instructions: InstructionSet<T>) -> Self {
        Self { stack: Stack::new(stack_size), text: instructions }
    }

    pub fn exec(mut self) -> Result<Stack, RuntimeError> {
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
                        let size = *(self.stack.read(self.stack.len - SLOT_SIZE)? as *const usize);
                        self.stack.pop(SLOT_SIZE)?;
                        let ptr = *(self.stack.read(self.stack.len - SLOT_SIZE)? as *const usize);
                        let ptr = ptr as *mut u8;
                        self.stack.pop(SLOT_SIZE)?;
                        let data = self.stack.read(self.stack.len - SLOT_SIZE * size)?;
                        self.stack.pop(SLOT_SIZE * size)?;

                        std::ptr::copy_nonoverlapping(data, ptr, size);

                        self.stack.push(ptr)?;
                    };
                },
                Instruction::InvokeNative(id) => {
                    match id {
                        // print
                        0x0001 => args!((f64) in self.stack, f64 => println!("{f64}")),
                        _ => panic!("unknown native function with id {id:#04X}"),
                    }
                },
                Instruction::NULL => break,
            };
        };

        Ok(self.stack)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Stack {
    ptr: *mut u8,
    cap: usize,
    len: usize,
}

impl Deref for Stack {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        unsafe {
            std::slice::from_raw_parts(self.ptr, self.cap)
        }
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, byte) in self.iter().enumerate() {
            if i != 0 {
                if i % 16 == 0 { writeln!(f)?; }
                else if i % 8 == 0 { write!(f, " ")?; };
            };
            write!(f, "{byte:02X} ")?;
        }
        Ok(())
    }
}

impl Stack {
    pub fn new(size: usize) -> Self {
        let layout = unsafe { Layout::from_size_align_unchecked(size, SLOT_SIZE) };
        let ptr = unsafe { std::alloc::alloc_zeroed(layout) };
        if ptr.is_null() { std::alloc::handle_alloc_error(layout); };

        Self {
            ptr,
            cap: size,
            len: 0,
        }
    }

    pub fn read(&self, addr: Address) -> Result<*const u8, AccessUndefinedError> {
        if addr > self.len { return Err(AccessUndefinedError); };

        unsafe { Ok(self.ptr.add(addr)) }
    }

    pub fn push<T>(&mut self, item: T) -> Result<usize, StackOverflowError> {
        let size = mem::size_of::<T>();
        if size > SLOT_SIZE {
            panic!("attempted to push more than {SLOT_SIZE} bytes");
        }

        let before = self.len;
        self.len += SLOT_SIZE;

        if self.len >= self.cap { return Err(StackOverflowError); };

        unsafe {
            std::ptr::write(self.ptr.add(before) as *mut T, item);
        };

        Ok(size)
    }

    pub fn pop(&mut self, len: usize) -> Result<(), StackUnderflowError> {
        if len > self.len { return Err(StackUnderflowError); };
        self.len -= len;
        Ok(())
    }

    pub fn copy(&mut self, src: Address, len: usize, dst: Address) -> Result<(), CopyError> {
        if src + len > self.len { return Err(CopyError::AccessUndefined(AccessUndefinedError)); };
        if dst + len > self.len { return Err(CopyError::AccessUndefined(AccessUndefinedError)); };

        if src + len > dst { return Err(CopyError::Overlapping); };

        unsafe {
            std::ptr::copy_nonoverlapping(self.ptr.add(src), self.ptr.add(dst), len);
        };

        Ok(())
    }
}

fn align_up(addr: *const u8, align: usize) -> *const u8 {
    let addr = addr as usize;
    ((addr + align - 1) & !(align - 1)) as *const u8
}

