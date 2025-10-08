use crate::Address;

#[macro_export]
macro_rules! inst {
    ($inst: expr; PUSHB $b: expr) => {
        $inst.push($crate::instruction::Instruction::Pushu8($b))
    };
    ($inst: expr; PUSHU $u: expr) => {
        $inst.push($crate::instruction::Instruction::Pushusize($u))
    };
    ($inst: expr; PUSHF $f: expr) => {
        $inst.push($crate::instruction::Instruction::Pushf64($f))
    };
    ($inst: expr; POP $u: expr) => {
        $inst.push($crate::instruction::Instruction::Pop($u))
    };
    ($inst: expr; COPY $f: expr, $s: expr, $t: expr) => {
        $inst.push($crate::instruction::Instruction::Copy($f, $s, $t))
    };
    ($inst: expr; ADDF) => {
        $inst.push($crate::instruction::Instruction::Addf)
    };
    ($inst: expr; SUBF) => {
        $inst.push($crate::instruction::Instruction::Subf)
    };
    ($inst: expr; MULTF) => {
        $inst.push($crate::instruction::Instruction::Multf)
    };
    ($inst: expr; DIVF) => {
        $inst.push($crate::instruction::Instruction::Divf)
    };
    ($inst: expr; MODF) => {
        $inst.push($crate::instruction::Instruction::Modf)
    };
    ($inst: expr; NEGF) => {
        $inst.push($crate::instruction::Instruction::Negf)
    };
    ($inst: expr; NOT) => {
        $inst.push($crate::instruction::Instruction::Not)
    };
    ($inst: expr; BPUSH $b: expr) => {
        $inst.push($crate::instruction::Instruction::BPush($b))
    };
    ($inst: expr; ALLOC) => {
        $inst.push($crate::instruction::Instruction::Alloc)
    };
    ($inst: expr; DEALLOC) => {
        $inst.push($crate::instruction::Instruction::Dealloc)
    };
    ($inst: expr; WRITE) => {
        $inst.push($crate::instruction::Instruction::Write)
    };
    ($inst: expr; INV_NATIVE $n: expr) => {
        $inst.push($crate::instruction::Instruction::InvokeNative($n))
    };
    ($inst: expr; STACK_PUSH) => {
        $inst.push($crate::instruction::Instruction::StackPush)
    };
    ($inst: expr; STACK_POP) => {
        $inst.push($crate::instruction::Instruction::StackPop)
    };

    ($inst: expr; $([$($tt: tt)+])*) => {
        $($crate::inst!($inst; $($tt)+));*
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstructionSet {
    pub inner: Vec<Instruction>,
}

impl Default for InstructionSet {
    fn default() -> Self {
        Self::new()
    }
}

impl InstructionSet {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
        }
    }

    pub fn push(&mut self, instruction: Instruction) {
        self.inner.push(instruction);
    }
}

impl IntoIterator for InstructionSet {
    type IntoIter = std::vec::IntoIter<Instruction>;
    type Item = Instruction;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Pushu8(u8),
    Pushusize(usize),
    Pushf64(f64),
    Pop(usize),
    Copy(Address, usize, Address),
    /// POP     f64: rhs
    /// POP     f64: lhs
    /// PUSH    f64: result
    Addf,
    /// f64 binary op, see [Instruction::Addf]
    Subf,
    /// f64 binary op, see [Instruction::Addf]
    Multf,
    /// f64 binary op, see [Instruction::Addf]
    Divf,
    /// f64 binary op, see [Instruction::Addf]
    Modf,
    /// POP     f64: value
    /// PUSH    f64: -value
    Negf,
    /// POP     bool: value
    /// PUSH    bool: !value
    Not,
    /// BYTE P  u8: value
    BPush(u8),
    /// POP     usize: size
    /// POP     usize: alignment
    /// PUSH    usize: heap data ptr
    Alloc,
    /// POP     usize: heap data ptr
    Dealloc,
    /// POP     usize: ptr
    /// BYTE FLUSH
    /// PUSH    usize: heap data ptr
    Write,
    InvokeNative(u32),

    StackPush,
    StackPop,
}

