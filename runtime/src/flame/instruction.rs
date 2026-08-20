use std::{fmt::{Display, Formatter}, ops::{Deref, DerefMut}};

#[macro_export]
macro_rules! inst {
    (PUSHU $b: expr) => {
        $crate::flame::instruction::Instruction::Pushusize($b)
    };
    (PUSHI $i: expr) => {
        $crate::flame::instruction::Instruction::Pushi64($i)
    };
    (PUSHF $f: expr) => {
        $crate::flame::instruction::Instruction::Pushf64($f)
    };
    (POP) => {
        $crate::flame::instruction::Instruction::Pop
    };
    (ADDF) => {
        $crate::flame::instruction::Instruction::Addf
    };
    (ADDI) => {
        $crate::flame::instruction::Instruction::Addi
    };
    (SUBF) => {
        $crate::flame::instruction::Instruction::Subf
    };
    (SUBI) => {
        $crate::flame::instruction::Instruction::Subi
    };
    (MULTF) => {
        $crate::flame::instruction::Instruction::Multf
    };
    (MULTI) => {
        $crate::flame::instruction::Instruction::Multi
    };
    (DIVF) => {
        $crate::flame::instruction::Instruction::Divf
    };
    (DIVI) => {
        $crate::flame::instruction::Instruction::Divi
    };
    (MODF) => {
        $crate::flame::instruction::Instruction::Modf
    };
    (MODI) => {
        $crate::flame::instruction::Instruction::Modi
    };
    (NEGF) => {
        $crate::flame::instruction::Instruction::Negf
    };
    (NEGI) => {
        $crate::flame::instruction::Instruction::Negi
    };
    (FCOMP_LT) => {
        $crate::flame::instruction::Instruction::FCompareLt
    };
    (ICOMP_LT) => {
        $crate::flame::instruction::Instruction::ICompareLt
    };
    (FCOMP_LE) => {
        $crate::flame::instruction::Instruction::FCompareLe
    };
    (ICOMP_LE) => {
        $crate::flame::instruction::Instruction::ICompareLe
    };
    (FCOMP_GT) => {
        $crate::flame::instruction::Instruction::FCompareGt
    };
    (ICOMP_GT) => {
        $crate::flame::instruction::Instruction::ICompareGt
    };
    (FCOMP_GE) => {
        $crate::flame::instruction::Instruction::FCompareGe
    };
    (ICOMP_GE) => {
        $crate::flame::instruction::Instruction::ICompareGe
    };
    (FCOMP_EQ) => {
        $crate::flame::instruction::Instruction::FCompareEq
    };
    (ICOMP_EQ) => {
        $crate::flame::instruction::Instruction::ICompareEq
    };
    (NOT) => {
        $crate::flame::instruction::Instruction::Not
    };
    (ALLOC_OBJ $i: expr) => {
        $crate::flame::instruction::Instruction::AllocObj($i)
    };
    (ALLOC_STR $str: expr) => {
        $crate::flame::instruction::Instruction::AllocString($str)
    };
    (ALLOC_ARR $t: expr, $l: expr) => {
        $crate::flame::instruction::Instruction::AllocArray($t, $l)
    };
    (STORE_LOCAL $i: expr) => {
        $crate::flame::instruction::Instruction::StoreLocal($i)
    };
    (LOAD_LOCAL $i: expr) => {
        $crate::flame::instruction::Instruction::LoadLocal($i)
    };
    (RET) => {
        $crate::flame::instruction::Instruction::Return
    };
    (INV $i: expr) => {
        $crate::flame::instruction::Instruction::Invoke($i)
    };
    (INV_MET $i: expr) => {
        $crate::flame::instruction::Instruction::InvokeMethod($i)
    };
    (READ $l: expr) => {
        $crate::flame::instruction::Instruction::Read($l)
    };
    (WRITE $l: expr) => {
        $crate::flame::instruction::Instruction::Write($l)
    };
    (GOTO $j: expr) => {
        $crate::flame::instruction::Instruction::Goto($j)
    };
    (GOTO_IF_TRUE $j: expr) => {
        $crate::flame::instruction::Instruction::GotoIfTrue($j)
    };
    (GOTO_IF_FALSE $j: expr) => {
        $crate::flame::instruction::Instruction::GotoIfFalse($j)
    };
    (POP_GOTO_IF_TRUE $j: expr) => {
        $crate::flame::instruction::Instruction::PopGotoIfTrue($j)
    };
    (POP_GOTO_IF_FALSE $j: expr) => {
        $crate::flame::instruction::Instruction::PopGotoIfFalse($j)
    };
    (THRW) => {
        $crate::flame::instruction::Instruction::Throw
    };

    (with $frame: expr => $span: expr; $([$($tt: tt)+])*) => {{
        if $frame.line_table.last().is_none_or(|map| $span.line() > map.line) {
            $frame.line_table.push($crate::flame::scope::LineMap::new($frame.instructions.len(), $span.line()));
        }
        inst!($frame.instructions; $([$($tt)+])*);
    }};
    (with $frame: expr => $span: expr; $($tt: tt)+) => {{
        if $frame.line_table.last().is_none_or(|map| $span.line() > map.line) {
            $frame.line_table.push($crate::flame::scope::LineMap::new($frame.instructions.len(), $span.line()));
        }
        inst!($frame.instructions; $($tt)+);
    }};

    ($inst: expr; $([$($tt: tt)+])*) => {
        $($inst.push(inst!($($tt)+)));*
    };
    ($inst: expr; $($tt: tt)+) => {
        $inst.push(inst!($($tt)+))
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

impl Display for InstructionSet {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (i, inst) in self.iter().enumerate() {
            writeln!(f, "{i:<5}{inst}")?;
        }
        Ok(())
    }
}

impl Deref for InstructionSet {
    type Target = Vec<Instruction>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for InstructionSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl InstructionSet {
    pub fn new() -> Self {
        Self {
            inner: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Pushusize(usize),
    Pushi64(i64),
    Pushf64(f64),
    Pop,
    /// POP     f64: rhs
    /// POP     f64: lhs
    /// PUSH    f64: result
    Addf,
    Addi,
    /// f64 binary op, see [Instruction::Addf]
    Subf,
    Subi,
    /// f64 binary op, see [Instruction::Addf]
    Multf,
    Multi,
    /// f64 binary op, see [Instruction::Addf]
    Divf,
    Divi,
    /// f64 binary op, see [Instruction::Addf]
    Modf,
    Modi,
    /// POP     f64: value
    /// PUSH    f64: -value
    Negf,
    Negi,

    FCompareLt,
    ICompareLt,
    FCompareLe,
    ICompareLe,
    FCompareGt,
    ICompareGt,
    FCompareGe,
    ICompareGe,
    FCompareEq,
    ICompareEq,
    /// POP     bool: value
    /// PUSH    bool: !value
    Not,

    AllocObj(usize),
    // PERF: use something else to reduce size
    AllocString(String),
    AllocArray(usize, usize),

    StoreLocal(usize),
    LoadLocal(usize),

    Return,
    Throw,

    Invoke(usize),
    InvokeMethod(usize),

    Read(usize),
    Write(usize),

    Goto(usize),
    GotoIfTrue(usize),
    GotoIfFalse(usize),
    PopGotoIfTrue(usize),
    PopGotoIfFalse(usize),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pushusize(usize) => write!(f, "{:20}{usize}", "PUSHU"),
            Self::Pushi64(i64) => write!(f, "{:20}{i64}", "PUSHI"),
            Self::Pushf64(f64) => write!(f, "{:20}{f64}", "PUSHF"),
            Self::Pop => write!(f, "POP"),
            Self::Addf => write!(f, "ADDF"),
            Self::Addi => write!(f, "ADDI"),
            Self::Subf => write!(f, "SUBF"),
            Self::Subi => write!(f, "SUBI"),
            Self::Multf => write!(f, "MULTF"),
            Self::Multi => write!(f, "MULTI"),
            Self::Divf => write!(f, "DIVF"),
            Self::Divi => write!(f, "DIVI"),
            Self::Modf => write!(f, "MODF"),
            Self::Modi => write!(f, "MODI"),
            Self::Negf => write!(f, "NEGF"),
            Self::Negi => write!(f, "NEGI"),
            Self::FCompareLt => write!(f, "FCOMP_LT"),
            Self::ICompareLt => write!(f, "ICOMP_LT"),
            Self::FCompareLe => write!(f, "FCOMP_LE"),
            Self::ICompareLe => write!(f, "ICOMP_LE"),
            Self::FCompareGt => write!(f, "FCOMP_GT"),
            Self::ICompareGt => write!(f, "ICOMP_GT"),
            Self::FCompareGe => write!(f, "FCOMP_GE"),
            Self::ICompareGe => write!(f, "ICOMP_GE"),
            Self::FCompareEq => write!(f, "FCOMP_EQ"),
            Self::ICompareEq => write!(f, "ICOMP_EQ"),
            Self::Not => write!(f, "NOT"),
            Self::Throw => write!(f, "THRW"),
            Self::AllocObj(index) => write!(f, "{:20}{index}", "ALLOC_OBJ"),
            Self::AllocString(str) => write!(f, "{:20}{str:?}", "ALLOC_STR"),
            Self::AllocArray(index, len) => write!(f, "{:20}{index} {len}", "ALLOC_ARRAY"),
            Self::StoreLocal(index) => write!(f, "{:20}{index}", "STORE_LOCAL"),
            Self::LoadLocal(index) => write!(f, "{:20}{index}", "LOAD_LOCAL"),
            Self::Return => write!(f, "RET"),
            Self::Invoke(num_args) => write!(f, "{:20}{num_args}", "INV"),
            Self::InvokeMethod(num_args) => write!(f, "{:20}{num_args}", "INV_MET"),
            Self::Read(len) => write!(f, "{:20}{len}", "READ"),
            Self::Write(len) => write!(f, "{:20}{len}", "WRITE"),
            Self::Goto(index) => write!(f, "{:20}{index}", "GOTO"),
            Self::GotoIfTrue(index) => write!(f, "{:20}{index}", "GOTO_IF_TRUE"),
            Self::GotoIfFalse(index) => write!(f, "{:20}{index}", "GOTO_IF_FALSE"),
            Self::PopGotoIfTrue(index) => write!(f, "{:20}{index}", "POP_GOTO_IF_TRUE"),
            Self::PopGotoIfFalse(index) => write!(f, "{:20}{index}", "POP_GOTO_IF_FALSE"),
        }
    }
}

