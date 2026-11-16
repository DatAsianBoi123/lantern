use std::{error::Error, fmt::{Display, Formatter}, ptr};

use error::RuntimeError;
use flame::{GeneratedFunction, instruction::Instruction};
use spark::{Spark, scope::GlobalVariables, ty::BuiltinType};

use crate::{error::{OutOfBoundsError, UserError}, flame::{FunctionKind, Globals}, heap::{GlobalStorage, Heap, HeapArray, HeapObject, TypeInfo}, stack::LanternStack};

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
        unsafe { self.0.ptr.cast() }
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

#[derive(Debug)]
pub struct Program {
    entrypoint: usize,
    globals: GlobalStorage,
    funs: Box<[GeneratedFunction]>,
    types: Box<[TypeInfo]>,
    #[allow(unused)]
    builtin_type_indices: [usize; BuiltinType::SIZE],
}

impl Program {
    pub const BYTE_ARR_TYPE_INDEX: usize = 0;
    pub const PRIMITIVE_ARR_TYPE_INDEX: usize = 1;
    pub const REF_ARR_TYPE_INDEX: usize = 2;

    pub fn new(spark: Spark) -> Self {
        let mut globals = Globals {
            funs: Vec::new(),
            // TODO: better way of builtin array type infos
            types: vec![
                TypeInfo::Array { element_size: 1, is_ref: false },
                TypeInfo::Array { element_size: 8, is_ref: false },
                TypeInfo::Array { element_size: size_of::<usize>(), is_ref: true },
            ],
            vars: GlobalVariables::new(),
        };
        let (entrypoint, builtin_type_indices) = flame::ignite(spark, &mut globals);
        let types = globals.types.into_boxed_slice();
        Self {
            entrypoint,
            globals: GlobalStorage::allocate(
                globals.vars,
                &types[Self::BYTE_ARR_TYPE_INDEX],
            ),
            funs: globals.funs.into_boxed_slice(),
            types,
            builtin_type_indices,
        }
    }

    pub fn funs(&self) -> &[GeneratedFunction] {
        &self.funs
    }
}

pub struct VM {
    stack: LanternStack,
    frames: Vec<Frame>,
    pub heap: Heap,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: LanternStack::new(2048),
            frames: Vec::with_capacity(512),
            // 4 MiB
            heap: Heap::new(4 * 2usize.pow(20)),
        }
    }

    pub fn stack(&self) -> &LanternStack {
        &self.stack
    }

    pub fn frames(&self) -> &[Frame] {
        &self.frames
    }

    pub fn alloc_obj(heap: &mut Heap, stack: &mut LanternStack, type_info: &TypeInfo) -> HeapObject {
        heap.alloc_obj(type_info).unwrap_or_else(|| {
            heap.gc(stack);
            while heap.used_space() >= 0.7 {
                heap.grow(stack);
            }
            heap.alloc_obj(type_info).unwrap()
        })
    }

    pub fn alloc_array(heap: &mut Heap, stack: &mut LanternStack, len: usize, type_info: &TypeInfo) -> HeapArray {
        heap.alloc_array(len, type_info).unwrap_or_else(|| {
            heap.gc(stack);
            while heap.used_space() >= 0.7 {
                heap.grow(stack);
            }
            heap.alloc_array(len, type_info).unwrap()
        })
    }

    pub fn alloc_string(&mut self, program: &Program, bytes: &[u8]) -> Result<HeapArray, RuntimeError> {
        let mut string = self.alloc_string_zeroed(program, bytes.len())?;
        unsafe { ptr::copy_nonoverlapping(bytes.as_ptr(), string.element_ptr_mut(), bytes.len()) };
        Ok(string)
    }

    pub fn alloc_string_zeroed(&mut self, program: &Program, len: usize) -> Result<HeapArray, RuntimeError> {
        let byte_type_info = &program.types[Program::BYTE_ARR_TYPE_INDEX];
        let str = Self::alloc_array(&mut self.heap, &mut self.stack, len, byte_type_info);
        Ok(str)
    }

    pub fn throw(&mut self, program: &Program, message: impl ToString) -> RuntimeError {
        let mut stacktrace = Vec::with_capacity(self.frames.len());
        while let Some(frame) = self.frames.pop() {
            let fun = &program.funs[frame.fun_index];
            stacktrace.push((fun.name.to_string(), fun.line_for(frame.inst_ptr)));
        }
        RuntimeError {
            message: message.to_string(),
            stacktrace,
        }
    }

    pub fn exec(&mut self, program: &Program) -> Result<(), RuntimeError> {
        self.frames.push(Frame::new(program.entrypoint, 0));
        'outer: while let Some(frame) = self.frames.last_mut() {
            let fun = &program.funs[frame.fun_index];
            let ret = match &fun.kind {
                FunctionKind::Instructions(instructions, locals) => {
                    let bottom = frame.bottom;
                    if let Err(err) = self.stack.reserve_to(bottom + *locals) {
                        return Err(self.throw(program, err));
                    }
                    loop {
                        let instruction = instructions[frame.inst_ptr];
                        match Self::exec_one(&mut self.stack, &mut self.heap, program, bottom, instruction) {
                            Ok(ExecutionFlow::Call(index, bottom)) => {
                                frame.inst_ptr += 1;
                                self.frames.push(Frame::new(index, bottom));
                                continue 'outer
                            }
                            Ok(ExecutionFlow::Goto(ip)) => frame.inst_ptr = ip,
                            Ok(ExecutionFlow::Continue) => frame.inst_ptr += 1,
                            Ok(ExecutionFlow::Return(slot)) => break slot,
                            Err(err) => return Err(self.throw(program, err)),
                        }
                    }
                }
                FunctionKind::Native(ptr) => ptr(self, program)?,
            };
            let frame = self.frames.pop().expect("last frame");
            if !self.frames.is_empty() {
                self.stack.shrink_to(frame.bottom - 1);
            }
            self.stack.push_slot(ret).map_err(|err| self.throw(program, err))?;
        }
        Ok(())
    }

    fn exec_one(
        stack: &mut LanternStack,
        heap: &mut Heap,
        program: &Program,
        bottom: usize,
        inst: Instruction,
    ) -> Result<ExecutionFlow, Box<dyn Error>> {
        match inst {
            Instruction::Pushusize(u64) => stack.push_usize(u64)?,
            Instruction::Pushi64(i64) => stack.push_int(i64)?,
            Instruction::Pushf64(f64) => stack.push_float(f64)?,
            Instruction::PushNull => stack.push_ref(ptr::null_mut())?,
            Instruction::Pop => { stack.pop()?; },
            Instruction::Dup(depth) => { stack.push_slot(stack[stack.top() - 1 - depth])?; },
            Instruction::Addf => args!((f64, f64) -> f64 in stack, (rhs, lhs) => lhs + rhs),
            Instruction::Addi => args!((i64, i64) -> i64 in stack, (rhs, lhs) => lhs.wrapping_add(rhs)),
            Instruction::Subf => args!((f64, f64) -> f64 in stack, (rhs, lhs) => lhs - rhs),
            Instruction::Subi => args!((i64, i64) -> i64 in stack, (rhs, lhs) => lhs.wrapping_sub(rhs)),
            Instruction::Multf => args!((f64, f64) -> f64 in stack, (rhs, lhs) => lhs * rhs),
            Instruction::Multi => args!((i64, i64) -> i64 in stack, (rhs, lhs) => lhs.wrapping_mul(rhs)),
            Instruction::Divf => args!((f64, f64) -> f64 in stack, (rhs, lhs) => lhs / rhs),
            Instruction::Divi => args!((i64, i64) -> i64 in stack, (rhs, lhs) => match lhs.checked_div(rhs) {
                Some(res) => res,
                None => return Err(DivisionError.into()),
            }),
            Instruction::Modf => args!((f64, f64) -> f64 in stack, (rhs, lhs) => lhs % rhs),
            Instruction::Modi => args!((i64, i64) -> i64 in stack, (rhs, lhs) => match lhs.checked_rem(rhs) {
                Some(res) => res,
                None => return Err(DivisionError.into()),
            }),
            Instruction::Negf => args!((f64) -> f64 in stack, rhs => -rhs),
            Instruction::Negi => args!((i64) -> i64 in stack, rhs => -rhs),
            Instruction::FCompareLt => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
            Instruction::ICompareLt => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs < rhs)),
            Instruction::FCompareLe => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
            Instruction::ICompareLe => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs <= rhs)),
            Instruction::FCompareGt => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
            Instruction::ICompareGt => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs > rhs)),
            Instruction::FCompareGe => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
            Instruction::ICompareGe => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs >= rhs)),
            Instruction::FCompareEq => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
            Instruction::ICompareEq => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs == rhs)),
            Instruction::FCompareNeq => args!((f64, f64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs != rhs)),
            Instruction::ICompareNeq => args!((i64, i64) -> usize in stack, (rhs, lhs) => bool_to_slot(lhs != rhs)),
            Instruction::IsNull => {
                let slot = stack.peek()?;
                let is_null = unsafe { matches!(slot.kind(), SlotType::Ref) && slot.read_ptr::<u8>().is_null() };
                stack.push_usize(bool_to_slot(is_null))?;
            },
            Instruction::Not => args!((usize) -> usize in stack, bool => match bool {
                0 => bool_to_slot(true),
                1 => bool_to_slot(false),
                _ => unreachable!(),
            }),
            Instruction::AllocObj(index) => {
                let mut obj = Self::alloc_obj(heap, stack, &program.types[index]);
                stack.push_ref(obj.as_mut_ptr())?;
            },
            Instruction::AllocArray(index, len) => {
                let mut array = Self::alloc_array(heap, stack, len, &program.types[index]);
                for i in 1..=len {
                    let slot = stack.pop()?;
                    let element = &slot.0 as *const _ as *const u8;
                    unsafe { array.set(len - i, element); }
                }
                stack.push_ref(array.as_mut_ptr())?;
            },
            Instruction::LoadLocal(index) => stack.push_slot(stack[bottom + index])?,
            Instruction::StoreLocal(index) => {
                // don't pop since assignment is an expression, value will be popped
                stack[bottom + index] = stack.peek()?;
            },
            Instruction::LoadGlobal(index) => stack.push_ref(program.globals[index])?,
            Instruction::Return => {
                let ret = stack.pop()?;
                return Ok(ExecutionFlow::Return(ret));
            },
            Instruction::Throw => {
                let string = unsafe { HeapArray::from_raw(stack.pop()?.read_ptr()) };
                let message = unsafe { std::str::from_utf8_unchecked(std::slice::from_raw_parts(string.element_ptr(), string.len())) };
                return Err(Box::new(UserError(message.to_string())))
            },
            Instruction::Invoke(num_args) => {
                // ARG_n
                // ARG_2
                // ARG_1 (bottom)
                // FUN_IDX
                let bottom = stack.top() - num_args;
                let index = unsafe { stack[bottom - 1].read_usize() };
                return Ok(ExecutionFlow::Call(index, bottom));
            },
            Instruction::InvokeMethod(num_args) => {
                // ARG_n
                // ARG_2
                // ARG_1
                // FUN_IDX (bottom, RECV copied here)
                // RECV
                let bottom = stack.top() - num_args - 1;
                let index_slot = &mut stack[bottom];
                let index = unsafe { index_slot.read_usize() };
                stack[bottom] = stack[bottom - 1];
                return Ok(ExecutionFlow::Call(index, bottom));
            },
            Instruction::Read(len) => {
                let offset = unsafe { stack.pop()?.read_usize() };
                let head = unsafe { stack.pop()?.read_ptr::<u8>() };
                let ptr = unsafe { head.add(offset) };

                if len == 0 {
                    // reference
                    stack.push_ref(unsafe { *ptr.cast() })?;
                } else {
                    // primitive
                    let slice = unsafe { std::slice::from_raw_parts(ptr, len) };
                    let mut field_bytes = [0; 8];
                    let (data, _) = field_bytes.split_at_mut(slice.len());
                    data.copy_from_slice(slice);
                    // since exact type cannot be determined statically, use the largest
                    // type instead
                    let element = i64::from_ne_bytes(field_bytes);
                    stack.push_int(element)?;
                }
            },
            Instruction::Write(len) => {
                let src = stack.pop()?;
                let src = &raw const src.0 as *const _;
                let offset = unsafe { stack.pop()?.read_usize() };
                let ptr = unsafe { stack.pop()?.read_ptr::<u8>() };

                unsafe { ptr.add(offset).copy_from(src, len); };

                stack.push_ref(ptr)?;
            },
            Instruction::Index => {
                let index = unsafe { stack.pop()?.read_int() };
                let array = unsafe { HeapArray::from_raw(stack.pop()?.read_ptr()) };

                let len = array.len();
                let ptr = index.try_into()
                    .ok()
                    .and_then(|index| array.get_sized(index))
                    .ok_or(OutOfBoundsError(index, len))?;

                if array.is_ref() {
                    stack.push_ref(unsafe { *ptr.cast() })?;
                } else {
                    let slice = unsafe { std::slice::from_raw_parts(ptr, array.element_size()) };
                    let mut element_bytes = [0; 8];
                    let (data, _) = element_bytes.split_at_mut(slice.len());
                    data.copy_from_slice(slice);
                    // since exact type cannot be determined statically, use the largest
                    // type instead
                    let element = i64::from_ne_bytes(element_bytes);
                    stack.push_int(element)?;
                }
            },
            Instruction::WriteIndex => {
                let src = stack.pop()?;
                let src = &raw const src.0 as *const _;
                let index = unsafe { stack.pop()?.read_int() };
                let mut array = unsafe { HeapArray::from_raw(stack.pop()?.read_ptr()) };

                let ptr = index.try_into()
                    .ok()
                    .and_then(|index| array.get_sized_mut(index))
                    .ok_or(OutOfBoundsError(index, array.len()))?;

                unsafe { ptr.copy_from(src, array.element_size()); }

                stack.push_ref(array.as_mut_ptr())?;
            },
            Instruction::Goto(ip) => {
                return Ok(ExecutionFlow::Goto(ip));
            },
            Instruction::GotoIfTrue(ip) => {
                if unsafe { bool_from_slot(stack.peek()?) } {
                    return Ok(ExecutionFlow::Goto(ip));
                }
            },
            Instruction::GotoIfFalse(ip) => {
                if unsafe { !bool_from_slot(stack.peek()?) } {
                    return Ok(ExecutionFlow::Goto(ip));
                }
            },
            Instruction::PopGotoIfTrue(ip) => {
                if unsafe { bool_from_slot(stack.pop()?) } {
                    return Ok(ExecutionFlow::Goto(ip));
                }
            },
            Instruction::PopGotoIfFalse(ip) => {
                if unsafe { !bool_from_slot(stack.pop()?) } {
                    return Ok(ExecutionFlow::Goto(ip));
                }
            },
            Instruction::Error => panic!("corrupted codegen, please report this!"),
        }

        Ok(ExecutionFlow::Continue)
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(thiserror::Error, Debug, Clone, Copy)]
#[error("division by 0")]
pub struct DivisionError;

#[derive(Clone, Copy)]
pub enum ExecutionFlow {
    Return(Slot),
    Call(usize, usize),
    Goto(usize),
    Continue,
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

