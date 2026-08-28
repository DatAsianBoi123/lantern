use std::{error::Error, fmt::{Display, Formatter}};

use diagnostic::{DiagnosticSink, symbol::SymbolTable};
use error::RuntimeError;
use flame::{GeneratedFunction, instruction::Instruction};
use parse::LanternFile;

use crate::{error::UserError, flame::{FunctionKind, scope::Globals}, heap::{Heap, HeapArray, TypeInfo}, stack::LanternStack};

macro_rules! args {
    (@pop usize, $stack: expr) => {
        unsafe { $stack.pop()?.read_usize() }
    };
    (@pop i64, $stack: expr) => {
        unsafe { $stack.pop()?.read_int() }
    };
    (@pop f64, $stack: expr) => {
        unsafe { $stack.pop()?.read_float() }
    };
    (@push usize, $stack: expr, $ret: expr) => {
        $stack.push_usize($ret)?
    };
    (@push i64, $stack: expr, $ret: expr) => {
        $stack.push_int($ret)?
    };
    (@push f64, $stack: expr, $ret: expr) => {
        $stack.push_float($ret)?
    };
    ( ( $($ty: tt),+ $(,)? ) -> $ret_ty: tt in $stack: expr, $pat: pat => $ret: expr) => {{
        let args = ( $( args!(@pop $ty, $stack) ),+ );

        let $pat = args;
        args!(@push $ret_ty, $stack, $ret)
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

#[repr(C)]
#[derive(Clone, Copy)]
pub union SlotData {
    usize: usize,
    int: i64,
    float: f64,
    ptr: *mut u8,
}

#[derive(Clone, Copy)]
pub struct Slot(SlotData, SlotType);

impl Display for Slot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:0>16x}", unsafe { self.0.int })?;
        Ok(())
    }
}

impl Slot {
    pub fn new_usize(usize: usize) -> Self {
        Self(SlotData { usize }, SlotType::Primitive)
    }

    pub fn new_int(int: i64) -> Self {
        Self(SlotData { int }, SlotType::Primitive)
    }

    pub fn new_float(float: f64) -> Self {
        Self(SlotData { float }, SlotType::Primitive)
    }

    pub fn new_ref(ptr: *mut u8) -> Self {
        Self(SlotData { ptr }, SlotType::Ref)
    }

    pub fn write_ref(&mut self, ptr: *mut u8) {
        self.0 = SlotData { ptr };
        self.1 = SlotType::Ref;
    }

    /// # Safety
    /// The Slot must actually contain a ptr to T
    pub unsafe fn read_ptr<T>(&self) -> *mut T {
        unsafe {
            self.0.ptr.cast()
        }
    }

    /// # Safety
    /// The Slot must actually contain a usize
    pub unsafe fn read_usize(&self) -> usize {
        unsafe { self.0.usize }
    }

    /// # Safety
    /// The Slot must actually contain an i64
    pub unsafe fn read_int(&self) -> i64 {
        unsafe { self.0.int }
    }

    /// # Safety
    /// The Slot must actually contain an f64
    pub unsafe fn read_float(&self) -> f64 {
        unsafe { self.0.float }
    }

    pub fn kind(&self) -> SlotType {
        self.1
    }
}

pub struct VM {
    stack: LanternStack,
    frames: Vec<Frame>,
    funs: Box<[GeneratedFunction]>,
    types: Box<[TypeInfo]>,
    pub heap: Heap,
}

impl VM {
    pub const STRING_TYPE_INDEX: usize = 0;
    pub const PRIMITIVE_ARR_TYPE_INDEX: usize = 1;
    pub const REF_ARR_TYPE_INDEX: usize = 2;

    pub fn new(file: LanternFile, sink: &mut DiagnosticSink, symbol_table: &SymbolTable) -> Option<Self> {
        let mut globals = Globals {
            funs: Vec::new(),
            // TODO: better way of array type info
            types: vec![TypeInfo::Array { element_size: 1, is_ref: false }, TypeInfo::Array { element_size: 8, is_ref: false }, TypeInfo::Array { element_size: size_of::<usize>(), is_ref: true }],
        };
        let root = flame::ignite(file, &mut globals, sink, symbol_table);
        if sink.fatal() {
            return None;
        }
        let mut stack = LanternStack::new(2048);
        if let FunctionKind::Instructions(_, locals) = root.kind { 
            stack.reserve(locals).expect("too many locals");
        }
        globals.funs.push(root);
        let mut frames = Vec::with_capacity(512);
        frames.push(Frame::new(globals.funs.len() - 1, 0));
        Some(Self {
            stack,
            frames,
            funs: globals.funs.into_boxed_slice(),
            types: globals.types.into_boxed_slice(),
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

    pub fn throw(&mut self, message: impl ToString) -> RuntimeError {
        let mut stacktrace = Vec::with_capacity(self.frames.len());
        while let Some(frame) = self.frames.pop() {
            let fun = &self.funs[frame.fun_index];
            stacktrace.push((fun.name.to_string(), fun.line_for(frame.inst_ptr)));
        }
        RuntimeError {
            message: message.to_string(),
            stacktrace,
        }
    }

    pub fn exec(mut self) -> Result<(), RuntimeError> {
        while !self.frames.is_empty() {
            self.exec_one()?;
        }
        Ok(())
    }

    pub fn exec_one(&mut self) -> Result<(), RuntimeError> {
        match self.exec_one_inner() {
            Ok(_) => Ok(()),
            Err(err) => Err(self.throw(err)),
        }
    }

    fn exec_one_inner(&mut self) -> Result<(), Box<dyn Error>> {
        let Some(frame) = self.frames.last_mut() else { return Ok(()); };

        let fun = &self.funs[frame.fun_index];
        match fun.kind {
            FunctionKind::Instructions(ref instructions, _) => {
                match instructions[frame.inst_ptr].clone() {
                    Instruction::Pushusize(u64) => self.stack.push_usize(u64)?,
                    Instruction::Pushi64(i64) => self.stack.push_int(i64)?,
                    Instruction::Pushf64(f64) => self.stack.push_float(f64)?,
                    Instruction::Pop => { self.stack.pop()?; },
                    Instruction::Addf => args!((f64, f64) -> f64 in self.stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Addi => args!((i64, i64) -> i64 in self.stack, (rhs, lhs) => lhs + rhs),
                    Instruction::Subf => args!((f64, f64) -> f64 in self.stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Subi => args!((i64, i64) -> i64 in self.stack, (rhs, lhs) => lhs - rhs),
                    Instruction::Multf => args!((f64, f64) -> f64 in self.stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Multi => args!((i64, i64) -> i64 in self.stack, (rhs, lhs) => lhs * rhs),
                    Instruction::Divf => args!((f64, f64) -> f64 in self.stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Divi => args!((i64, i64) -> i64 in self.stack, (rhs, lhs) => lhs / rhs),
                    Instruction::Modf => args!((f64, f64) -> f64 in self.stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Modi => args!((i64, i64) -> i64 in self.stack, (rhs, lhs) => lhs % rhs),
                    Instruction::Negf => args!((f64) -> f64 in self.stack, rhs => -rhs),
                    Instruction::Negi => args!((i64) -> i64 in self.stack, rhs => -rhs),
                    Instruction::FCompareLt => args!((f64, f64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::ICompareLt => args!((i64, i64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
                    Instruction::FCompareLe => args!((f64, f64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::ICompareLe => args!((i64, i64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
                    Instruction::FCompareGt => args!((f64, f64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::ICompareGt => args!((i64, i64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
                    Instruction::FCompareGe => args!((f64, f64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::ICompareGe => args!((i64, i64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
                    Instruction::FCompareEq => args!((f64, f64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::ICompareEq => args!((i64, i64) -> usize in self.stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
                    Instruction::Not => args!((usize) -> usize in self.stack, bool => match bool {
                        0 => bool_to_slot(true),
                        1 => bool_to_slot(false),
                        _ => unreachable!(),
                    }),
                    Instruction::AllocObj(index) => {
                        // TODO: gc
                        let mut obj = self.heap.alloc_obj(&self.types[index]).unwrap();
                        self.stack.push_ref(obj.as_mut_ptr())?;
                    },
                    Instruction::AllocString(str) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(str.len(), &self.types[Self::STRING_TYPE_INDEX]).unwrap();
                        for (i, byte) in str.bytes().enumerate() {
                            unsafe { array.set(i, &byte as *const u8); }
                        }
                        self.stack.push_ref(array.as_mut_ptr())?;
                    },
                    Instruction::AllocArray(index, len) => {
                        // TODO: figure out when to GC
                        let mut array = self.heap.alloc_array(len, &self.types[index]).unwrap();
                        for i in 1..=len {
                            let element = &self.stack.pop()?.0 as *const _ as *const u8;
                            unsafe { array.set(len - i, element); }
                        }
                        self.stack.push_ref(array.as_mut_ptr())?;
                    },
                    Instruction::LoadLocal(index) => self.stack.push_slot(self.stack[frame.bottom + index])?,
                    Instruction::StoreLocal(index) => {
                        // don't pop since assignment is an expression, value will be popped
                        self.stack[frame.bottom + index] = self.stack.peek()?;
                    },
                    Instruction::Return => {
                        let ret = self.stack.pop()?;
                        let bottom = self.frames.pop().expect("frame exists").bottom;
                        // bottom-most slot is the function index
                        if !self.frames.is_empty() { self.stack.shrink_to(bottom - 1); };
                        self.stack.push_slot(ret)?;
                        return Ok(());
                    },
                    Instruction::Throw => {
                        let ptr = unsafe { HeapArray::from_raw(*self.stack.pop()?.read_ptr()) };
                        let message = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.element_ptr(), ptr.len())) };
                        return Err(Box::new(UserError(message.to_string())))
                    },
                    Instruction::Invoke(num_args) => {
                        // ARG_n
                        // ARG_2
                        // ARG_1 (bottom)
                        // FUN_IDX
                        frame.inst_ptr += 1;
                        let bottom = self.stack.top() - num_args;
                        let index = unsafe { self.stack[bottom - 1].read_usize() };
                        // TODO: find a better way to do this
                        if let FunctionKind::Instructions(_, locals) = self.funs[index].kind {
                            self.stack.reserve(locals - num_args)?;
                        }
                        self.frames.push(Frame::new(index, bottom));
                        return Ok(());
                    },
                    Instruction::InvokeMethod(num_args) => {
                        // ARG_n
                        // ARG_2
                        // ARG_1
                        // FUN_IDX (bottom, RECV copied here)
                        // RECV
                        frame.inst_ptr += 1;
                        let bottom = self.stack.top() - num_args - 1;
                        let index_slot = &mut self.stack[bottom];
                        let index = unsafe { index_slot.read_usize() };
                        // unsafe is needed here since Rust won't allow two mutable references to
                        // self.stack
                        unsafe { std::ptr::write(index_slot, self.stack[bottom - 1]); };
                        // TODO: find a better way to do this
                        if let FunctionKind::Instructions(_, locals) = self.funs[index].kind {
                            self.stack.reserve(locals - num_args - 1)?;
                        }
                        let frame = Frame::new(index, bottom);
                        self.frames.push(frame);
                        return Ok(());
                    },
                    Instruction::Read(len) => {
                        let offset = unsafe { self.stack.pop()?.read_usize() };
                        let head = unsafe { self.stack.pop()?.read_ptr::<u8>() };
                        let ptr = unsafe { head.add(offset) };

                        if len == 0 {
                            // reference
                            self.stack.push_ref(unsafe { *ptr.cast::<*mut u8>() })?;
                        } else {
                            // primitive
                            let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                            let mut field_bytes = [0; 8];
                            let (data, _) = field_bytes.split_at_mut(slice.len());
                            data.copy_from_slice(slice);
                            // since exact type cannot be determined statically, use the largest
                            // type instead
                            let element = i64::from_ne_bytes(field_bytes);
                            self.stack.push_int(element)?;
                        }
                    },
                    Instruction::Write(len) => {
                        let src = self.stack.pop()?;
                        let src = &raw const src.0 as *const _;
                        let offset = unsafe { self.stack.pop()?.read_usize() };
                        let ptr = unsafe { self.stack.pop()?.read_ptr::<u8>() };

                        unsafe { ptr.add(offset).copy_from(src, len); };

                        self.stack.push_ref(ptr)?;
                    },
                    Instruction::Goto(ptr) => {
                        frame.inst_ptr = ptr;
                        return Ok(());
                    },
                    Instruction::GotoIfTrue(ptr) => {
                        if unsafe { bool_from_slot(self.stack.peek()?) } {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::GotoIfFalse(ptr) => {
                        if unsafe { !bool_from_slot(self.stack.peek()?) } {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfTrue(ptr) => {
                        if unsafe { bool_from_slot(self.stack.pop()?) } {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                    Instruction::PopGotoIfFalse(ptr) => {
                        if unsafe { !bool_from_slot(self.stack.pop()?) } {
                            frame.inst_ptr = ptr;
                            return Ok(());
                        }
                    },
                }

                frame.inst_ptr += 1;
                Ok(())
            },
            FunctionKind::Native(ptr) => {
                let ret = ptr(self)?;

                let bottom = self.frames.pop().expect("frame exists").bottom;
                // bottom-most slot is the function index
                self.stack.shrink_to(bottom - 1);
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

const fn bool_to_slot(bool: bool) -> usize {
    if bool {
        1
    } else {
        0
    }
}

unsafe fn bool_from_slot(slot: Slot) -> bool {
    unsafe {
        match slot.read_usize() {
            1 => true,
            0 => false,
            _ => unreachable!(),
        }
    }
}

