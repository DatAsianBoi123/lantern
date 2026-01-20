#![feature(get_mut_unchecked)]

use std::{fmt::{Display, Formatter}};

use anyhow::anyhow;
use error::RuntimeError;
use flame::{GeneratedFunction, instruction::Instruction};
use parse::LanternFile;

use crate::{flame::{error::CompilerError, scope::Globals}, heap::{Heap, HeapArray, TypeInfo}, stack::LanternStack};

macro_rules! args {
    ( ( $($ty: ty),+ $(,)? ) in $stack: expr, $pat: pat => $ret: expr) => {{
        let args = ( $( unsafe { *($stack.pop()?.read::<$ty>()) } ),+ );

        let $pat = args;
        $stack.push_primitive($ret)?;
    }};
}

pub mod flame;
pub mod stack;
pub mod heap;
pub mod error;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum SlotType {
    #[default]
    Primitive,
    Ref,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slot(u64, SlotType);

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for byte in self.0.to_ne_bytes() {
            write!(f, "{byte:0<2x} ")?;
        }
        Ok(())
    }
}

impl Slot {
    pub fn new_primitive<T>(primitive: T) -> Self {
        let mut slot = Self(0, SlotType::Primitive);
        slot.write_primitive(primitive);
        slot
    }

    pub fn new_ref(ptr: *const u8) -> Self {
        Self(ptr as u64, SlotType::Ref)
    }

    pub fn write_ref(&mut self, ptr: *const u8) {
        self.0 = ptr as u64;
        self.1 = SlotType::Ref;
    }

    pub fn write_primitive<T>(&mut self, primitive: T) {
        if size_of::<T>() > 8 {
            panic!("attempted to write more than 8 bytes into a Slot");
        }

        self.1 = SlotType::Primitive;
        unsafe {
            (&mut self.0 as *mut _ as *mut T).write(primitive);
        }
    }

    pub fn read<T>(&self) -> *const T {
        &raw const self.0 as *const T
    }

    pub fn kind(&self) -> SlotType {
        self.1
    }
}

#[derive(Debug, Clone)]
pub struct VM {
    frames: Vec<Frame>,
    funs: Vec<GeneratedFunction>,
    types: Vec<TypeInfo>,
    pub heap: Heap,
}

impl VM {
    pub fn new(file: LanternFile) -> Result<Self, CompilerError> {
        let mut globals = Globals {
            funs: Vec::new(),
            // TODO: better way of array type info
            types: vec![TypeInfo::Array { element_size: 1, is_ref: false }, TypeInfo::Array { element_size: 8, is_ref: false }],
        };
        let root = flame::ignite(file, &mut globals)?;
        globals.funs.push(root);
        Ok(Self {
            frames: vec![Frame::new(globals.funs.len() - 1)],
            funs: globals.funs,
            types: globals.types,
            // 4 MiB
            heap: Heap::new(4 * 2usize.pow(20)),
        })
    }

    pub fn funs(&self) -> &[GeneratedFunction] {
        &self.funs
    }

    pub fn root(&self) -> &GeneratedFunction {
        self.funs.last().expect("root fun")
    }

    pub fn alloc_string(&mut self, bytes: &[u8]) -> Result<HeapArray, RuntimeError> {
        let mut array = self.heap.alloc_array(bytes.len(), &self.types[0])
            .unwrap_or_else(|| {
                self.heap.gc(&mut self.frames);
                self.heap.alloc_array(bytes.len(), &self.types[0]).expect("free heap space after gc")
            });
        for (i, byte) in bytes.iter().copied().enumerate() {
            unsafe { array.set(i, &byte as *const u8); }
        }
        Ok(array)
    }

    pub fn exec(mut self) -> Result<(), RuntimeError> {
        while !self.frames.is_empty() {
            self.exec_one()?;
        }
        Ok(())
    }

    pub fn exec_one(&mut self) -> Result<(), RuntimeError> {
        let Some(ref mut frame) = self.frames.last_mut() else { return Ok(()); };

        let fun = &self.funs[frame.fun_index];
        match fun {
            GeneratedFunction::Instructions(instructions) => {
                match instructions[frame.inst_ptr].clone() {
                    Instruction::Pushu64(u64) => { frame.operand_stack.push_primitive(u64)?; },
                    Instruction::Pushi64(i64) => { frame.operand_stack.push_primitive(i64)?; },
                    Instruction::Pushf64(f64) => { frame.operand_stack.push_primitive(f64)?; },
                    Instruction::Pop => { frame.operand_stack.pop()?; },
                    Instruction::Addf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Addi => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Subf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Subi => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Multf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Multi => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Divf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Divi => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Modf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Modi => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Negf => args!((f64) in frame.operand_stack, rhs => -rhs),
                    Instruction::Negi => args!((i64) in frame.operand_stack, rhs => -rhs),
                    Instruction::FCompareLt => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::ICompareLt => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::FCompareLe => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::ICompareLe => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::FCompareGt => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::ICompareGt => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::FCompareGe => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::ICompareGe => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::FCompareEq => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::ICompareEq => args!((i64, i64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::Not => args!((bool) in frame.operand_stack, bool => bool_to_slot(!bool)),
                    Instruction::AllocString(str) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(str.len(), &self.types[0]).unwrap();
                        for (i, byte) in str.bytes().enumerate() {
                            unsafe { array.set(i, &byte as *const u8); }
                        }
                        frame.operand_stack.push_ref(array.as_ptr())?;
                    },
                    Instruction::AllocArray(len) => {
                        let type_index = unsafe { *frame.operand_stack.pop()?.read::<usize>() };
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(len, &self.types[type_index]).unwrap();
                        for i in 1..=len {
                            let element = &frame.operand_stack.pop()?.0 as *const _ as *const u8;
                            unsafe { array.set(len - i, element); }
                        }
                        frame.operand_stack.push_ref(array.as_ptr())?;
                    },
                    Instruction::LoadLocal(index) => frame.operand_stack.push_slot(frame.locals[index])?,
                    Instruction::StoreLocal(index) => frame.locals[index] = frame.operand_stack.pop()?,
                    Instruction::Return => {
                        let ret = frame.operand_stack.pop()?;
                        self.frames.pop();
                        if let Some(frame) = self.frames.last_mut() {
                            frame.operand_stack.push_slot(ret)?;
                        };
                        return Ok(());
                    },
                    Instruction::Invoke(num_args) => {
                        frame.inst_ptr += 1;
                        let mut locals = [Default::default(); 256];
                        for i in 0..num_args {
                            locals[num_args - i - 1] = frame.operand_stack.pop()?;
                        }
                        let index = unsafe { *frame.operand_stack.pop()?.read::<usize>() };
                        let frame = Frame::with_locals(index, locals);
                        self.frames.push(frame);
                        return Ok(());
                    },
                    Instruction::Index => {
                        let index = unsafe { *frame.operand_stack.pop()?.read::<i64>() };
                        let ptr = unsafe { *frame.operand_stack.pop()?.read::<*mut u8>() };

                        let array = unsafe { HeapArray::from_raw(ptr) };
                        let ptr = if index >= 0 {
                            array.get(index as usize).ok_or(RuntimeError(anyhow!("index out of bounds").into()))?
                        } else {
                            let index = array.len().checked_sub(index.unsigned_abs()as usize).ok_or(RuntimeError(anyhow!("index out of bounds").into()))?;
                            array.get(index).expect("index in range")
                        };
                        let slice = unsafe { std::slice::from_raw_parts(ptr, array.element_size()) };
                        let mut element_bytes = [0; 8];
                        let (data, _) = element_bytes.split_at_mut(slice.len());
                        data.copy_from_slice(slice);
                        let element = u64::from_ne_bytes(element_bytes);
                        if array.is_ref() {
                            frame.operand_stack.push_ref(element as *const u8)?;
                        } else {
                            frame.operand_stack.push_primitive(element)?;
                        }
                    },
                    Instruction::Goto(ptr) => {
                        frame.inst_ptr = ptr;
                        return Ok(());
                    },
                    Instruction::GotoIfTrue(ptr) => {
                        if bool_from_slot(frame.operand_stack.peek()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::GotoIfFalse(ptr) => {
                        if !bool_from_slot(frame.operand_stack.peek()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfTrue(ptr) => {
                        if bool_from_slot(frame.operand_stack.pop()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfFalse(ptr) => {
                        if !bool_from_slot(frame.operand_stack.pop()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                }

                frame.inst_ptr += 1;
                Ok(())
            },
            GeneratedFunction::Native(ptr) => {
                let frame = self.frames.pop().expect("frame");
                let ret = ptr(self, frame.locals)?;

                if let Some(frame) = self.frames.last_mut() {
                    frame.operand_stack.push_slot(ret)?;
                };
                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    fun_index: usize,
    inst_ptr: usize,
    locals: [Slot; 256],
    operand_stack: LanternStack,
}

impl Frame {
    pub fn new(fun_index: usize) -> Self {
        Self {
            fun_index,
            inst_ptr: 0,
            locals: [Default::default(); 256],
            operand_stack: LanternStack::new(),
        }
    }

    pub fn with_locals(fun_index: usize, locals: [Slot; 256]) -> Self {
        Self {
            fun_index,
            inst_ptr: 0,
            locals,
            operand_stack: LanternStack::new(),
        }
    }
}

fn bool_to_slot(bool: bool) -> u64 {
    if bool {
        1
    } else {
        0
    }
}

fn bool_from_slot(slot: Slot) -> bool {
    match slot.0 {
        1 => true,
        0 => false,
        _ => panic!("invalid bool {slot:?}"),
    }
}

