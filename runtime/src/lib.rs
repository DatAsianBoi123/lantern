use std::fmt::{Display, Formatter};

use diagnostic::DiagnosticSink;
use error::RuntimeError;
use flame::{GeneratedFunction, instruction::Instruction};
use parse::LanternFile;

use crate::{flame::scope::Globals, heap::{Heap, HeapArray, TypeInfo}, stack::LanternStack};

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
    // Primitive is 0 so GC doesn't think a zeroed Slot is a reference
    #[default]
    Primitive = 0,
    Ref = 1,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub struct Slot(u64, SlotType);

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0<16x}", self.0)?;
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
            (&raw mut self.0 as *mut T).write(primitive);
        }
    }

    pub fn read<T>(&self) -> *const T {
        &raw const self.0 as *const T
    }

    pub fn kind(&self) -> SlotType {
        self.1
    }
}

#[derive(Debug)]
pub struct VM {
    stack: LanternStack,
    frames: Vec<Frame>,
    funs: Vec<GeneratedFunction>,
    types: Vec<TypeInfo>,
    pub heap: Heap,
}

impl VM {
    pub const STRING_TYPE_INDEX: usize = 0;
    pub const PRIMITIVE_ARR_TYPE_INDEX: usize = 1;

    pub fn new(file: LanternFile, sink: &mut DiagnosticSink) -> Option<Self> {
        let mut globals = Globals {
            funs: Vec::new(),
            // TODO: better way of array type info
            types: vec![TypeInfo::Array { element_size: 1, is_ref: false }, TypeInfo::Array { element_size: 8, is_ref: false }],
        };
        let root = flame::ignite(file, &mut globals, sink);
        if sink.fatal() {
            return None;
        }
        let mut stack = LanternStack::new(2048);
        if let GeneratedFunction::Instructions(_, locals) = root { 
            *stack.top_mut() = locals;
        }
        globals.funs.push(root);
        let mut frames = Vec::with_capacity(512);
        frames.push(Frame::new(globals.funs.len() - 1, 0));
        Some(Self {
            stack,
            frames,
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

    pub fn frames(&self) -> &[Frame] {
        &self.frames
    }

    pub fn stack(&self) -> &LanternStack {
        &self.stack
    }

    pub fn alloc_string(&mut self, bytes: &[u8]) -> Result<HeapArray, RuntimeError> {
        let mut array = self.heap.alloc_array(bytes.len(), &self.types[Self::STRING_TYPE_INDEX])
            .unwrap_or_else(|| {
                self.heap.gc(&mut self.stack);
                self.heap.alloc_array(bytes.len(), &self.types[Self::STRING_TYPE_INDEX]).expect("free heap space after gc")
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
        let Some(frame) = self.frames.last_mut() else { return Ok(()); };

        let fun = &self.funs[frame.fun_index];
        match fun {
            GeneratedFunction::Instructions(instructions, _) => {
                match instructions[frame.inst_ptr].clone() {
                    Instruction::Pushu64(u64) => self.stack.push_primitive(u64)?,
                    Instruction::Pushi64(i64) => self.stack.push_primitive(i64)?,
                    Instruction::Pushf64(f64) => self.stack.push_primitive(f64)?,
                    Instruction::Pop => { self.stack.pop()?; },
                    Instruction::Addf => args!((f64, f64) in self.stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Addi => args!((i64, i64) in self.stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Subf => args!((f64, f64) in self.stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Subi => args!((i64, i64) in self.stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Multf => args!((f64, f64) in self.stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Multi => args!((i64, i64) in self.stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Divf => args!((f64, f64) in self.stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Divi => args!((i64, i64) in self.stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Modf => args!((f64, f64) in self.stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Modi => args!((i64, i64) in self.stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Negf => args!((f64) in self.stack, rhs => -rhs),
                    Instruction::Negi => args!((i64) in self.stack, rhs => -rhs),
                    Instruction::FCompareLt => args!((f64, f64) in self.stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::ICompareLt => args!((i64, i64) in self.stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::FCompareLe => args!((f64, f64) in self.stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::ICompareLe => args!((i64, i64) in self.stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::FCompareGt => args!((f64, f64) in self.stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::ICompareGt => args!((i64, i64) in self.stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::FCompareGe => args!((f64, f64) in self.stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::ICompareGe => args!((i64, i64) in self.stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::FCompareEq => args!((f64, f64) in self.stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::ICompareEq => args!((i64, i64) in self.stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::Not => args!((bool) in self.stack, bool => bool_to_slot(!bool)),
                    Instruction::AllocObj(index) => {
                        // TODO: gc
                        let obj = self.heap.alloc_obj(&self.types[index]).unwrap();
                        self.stack.push_ref(obj.as_ptr())?;
                    },
                    Instruction::AllocString(str) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(str.len(), &self.types[Self::STRING_TYPE_INDEX]).unwrap();
                        for (i, byte) in str.bytes().enumerate() {
                            unsafe { array.set(i, &byte as *const u8); }
                        }
                        self.stack.push_ref(array.as_ptr())?;
                    },
                    Instruction::AllocArray(index, len) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(len, &self.types[index]).unwrap();
                        for i in 1..=len {
                            let element = &self.stack.pop()?.0 as *const _ as *const u8;
                            unsafe { array.set(len - i, element); }
                        }
                        self.stack.push_ref(array.as_ptr())?;
                    },
                    Instruction::LoadLocal(index) => self.stack.push_slot(self.stack[frame.bottom + index])?,
                    Instruction::StoreLocal(index) => {
                        // don't pop since assignment is an expression, value will be popped
                        self.stack[frame.bottom + index] = self.stack.peek()?;
                    },
                    Instruction::Return => {
                        let ret = self.stack.pop()?;
                        let bottom = self.frames.pop().expect("frame exists").bottom;
                        if !self.frames.is_empty() { *self.stack.top_mut() = bottom - 1; };
                        self.stack.push_slot(ret)?;
                        return Ok(());
                    },
                    Instruction::Invoke(num_args) => {
                        // ARG_n
                        // ARG_2
                        // ARG_1 (bottom)
                        // FUN_IDX
                        frame.inst_ptr += 1;
                        let bottom = self.stack.top() - num_args;
                        let index = unsafe { *self.stack[bottom - 1].read::<usize>() };
                        // TODO: find a better way to do this
                        if let GeneratedFunction::Instructions(_, locals) = self.funs[index] {
                            *self.stack.top_mut() += locals - num_args;
                        }
                        self.frames.push(Frame::new(index, bottom));
                        return Ok(());
                    },
                    Instruction::InvokeMethod(num_args) => {
                        // ARG_n
                        // ARG_2
                        // ARG_1
                        // FUN_IDX
                        // RECV (bottom)
                        frame.inst_ptr += 1;
                        let bottom = self.stack.top() - num_args - 2;
                        let index = unsafe { *self.stack[bottom + 1].read::<usize>() };
                        // copy args down so local indices match
                        unsafe { std::ptr::copy(&raw const self.stack[bottom + 2], &raw mut self.stack[bottom + 1], num_args); };
                        *self.stack.top_mut() -= 1;
                        // TODO: find a better way to do this
                        if let GeneratedFunction::Instructions(_, locals) = self.funs[index] {
                            *self.stack.top_mut() += locals - num_args - 1;
                        }
                        let frame = Frame::new(index, bottom);
                        self.frames.push(frame);
                        return Ok(());
                    },
                    Instruction::Read(len) => {
                        let ptr = unsafe { *self.stack.pop()?.read::<*const u8>() };

                        if len == 0 {
                            // reference
                            self.stack.push_ref(unsafe { *(ptr as *const *const u8) })?;
                        } else {
                            // primitive
                            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                            let mut field_bytes = [0; 8];
                            let (data, _) = field_bytes.split_at_mut(slice.len());
                            data.copy_from_slice(slice);
                            let element = u64::from_ne_bytes(field_bytes);
                            self.stack.push_primitive(element)?;
                        }
                    },
                    Instruction::Write(len) => {
                        let new_field = self.stack.pop()?.read::<u8>();
                        let offset = unsafe { *self.stack.pop()?.read::<usize>() };
                        let ptr = unsafe { *self.stack.pop()?.read::<*mut u8>() };

                        unsafe { ptr.add(offset).copy_from(new_field, len); };

                        self.stack.push_ref(ptr)?;
                    },
                    Instruction::Goto(ptr) => {
                        frame.inst_ptr = ptr;
                        return Ok(());
                    },
                    Instruction::GotoIfTrue(ptr) => {
                        if bool_from_slot(self.stack.peek()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::GotoIfFalse(ptr) => {
                        if !bool_from_slot(self.stack.peek()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfTrue(ptr) => {
                        if bool_from_slot(self.stack.pop()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfFalse(ptr) => {
                        if !bool_from_slot(self.stack.pop()?) {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                }

                frame.inst_ptr += 1;
                Ok(())
            },
            GeneratedFunction::Native(ptr) => {
                let ret = ptr(self)?;

                let bottom = self.frames.pop().expect("frame exists").bottom;
                *self.stack.top_mut() = bottom - 1;
                self.stack.push_slot(ret)?;
                Ok(())
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Frame {
    fun_index: usize,
    inst_ptr: usize,
    bottom: usize,
}

impl Frame {
    pub fn new(fun_index: usize, bottom: usize) -> Self {
        Self {
            fun_index,
            inst_ptr: 0,
            bottom,
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

