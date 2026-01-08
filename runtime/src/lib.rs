#![feature(get_mut_unchecked)]

use std::{fmt::{Display, Formatter}, rc::Rc};

use error::RuntimeError;
use flame::{GeneratedFunction, instruction::Instruction};
use parse::LanternFile;

use crate::{flame::error::CompilerError, heap::{Heap, HeapArray, TypeInfo}, stack::LanternStack};

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
pub struct VM<'a> {
    frames: Vec<Frame<'a>>,
    pub heap: Heap,

    string_type_info: TypeInfo,
}

impl<'a> Default for VM<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> VM<'a> {
    pub fn new() -> Self {
        Self {
            frames: Vec::new(),
            // 4 MiB
            heap: Heap::new(4 * 2usize.pow(20)),

            string_type_info: TypeInfo::Array { element_size: 1, is_ref: false },
        }
    }

    pub fn ignite(&mut self, file: LanternFile) -> Result<GeneratedFunction, CompilerError> {
        flame::ignite(file, &mut self.heap)
    }

    pub fn push_frame(&mut self, fun: &'a GeneratedFunction) {
        self.frames.push(Frame::new(fun));
    }

    pub fn alloc_string(&mut self, bytes: &[u8]) -> Result<HeapArray, RuntimeError> {
        let mut array = self.heap.alloc_array(bytes.len(), &self.string_type_info)
            .unwrap_or_else(|| {
                self.heap.gc(&mut self.frames);
                self.heap.alloc_array(bytes.len(), &self.string_type_info).expect("free heap space after gc")
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

        match frame.fun {
            GeneratedFunction::Instructions { instructions, funs } => {
                match instructions[frame.inst_ptr].clone() {
                    Instruction::Pushu64(u64) => { frame.operand_stack.push_primitive(u64)?; },
                    Instruction::Pushf64(f64) => { frame.operand_stack.push_primitive(f64)?; },
                    Instruction::Pop => { frame.operand_stack.pop()?; },
                    Instruction::Addf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Subf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Multf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Divf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Modf => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Negf => args!((f64) in frame.operand_stack, f64 => -f64),
                    Instruction::CompareLt => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::CompareLe => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::CompareGt => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::CompareGe => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::CompareEq => args!((f64, f64) in frame.operand_stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::Not => args!((bool) in frame.operand_stack, bool => bool_to_slot(!bool)),
                    Instruction::AllocString(str) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(str.len(), &self.string_type_info).unwrap();
                        for (i, byte) in str.bytes().enumerate() {
                            unsafe { array.set(i, &byte as *const u8); }
                        }
                        frame.operand_stack.push_ref(array.as_ptr())?;
                        println!("Allocated {} bytes @ {array:?}: {:?}, {:?}", array.size(), array.header(), array.type_info());
                    },
                    Instruction::LoadLocal(index) => frame.operand_stack.push_slot(frame.locals[index])?,
                    Instruction::StoreLocal(index) => frame.locals[index] = frame.operand_stack.pop()?,
                    // TODO: store functions on heap
                    Instruction::LoadFun(index) => frame.operand_stack.push_primitive(Rc::as_ptr(&funs[index]))?,
                    Instruction::Return => {
                        let ret = frame.operand_stack.pop()?;
                        if !frame.operand_stack.is_empty() {
                            println!("WARNING: Returning from a frame with data left on its operand stack!");
                            println!("{:?}", frame.operand_stack);
                        }
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
                        let ptr = frame.operand_stack.pop()?.read::<*const GeneratedFunction>();
                        let fun = unsafe { &**ptr };
                        let frame = Frame::with_locals(fun, locals);
                        self.frames.push(frame);
                        return Ok(());
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
pub struct Frame<'a> {
    fun: &'a GeneratedFunction,
    inst_ptr: usize,
    locals: [Slot; 256],
    operand_stack: LanternStack,
}

impl<'a> Frame<'a> {
    pub fn new(fun: &'a GeneratedFunction) -> Self {
        Self {
            fun,
            inst_ptr: 0,
            locals: [Default::default(); 256],
            operand_stack: LanternStack::new(),
        }
    }

    pub fn with_locals(fun: &'a GeneratedFunction, locals: [Slot; 256]) -> Self {
        Self {
            fun,
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

