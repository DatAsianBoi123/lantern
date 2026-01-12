use std::{fmt::{Display, Formatter}, ops::{Deref, DerefMut}};

#[macro_export]
macro_rules! inst {
    ($inst: expr; PUSHU $b: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Pushu64($b))
    };
    ($inst: expr; PUSHI $i: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Pushi64($i))
    };
    ($inst: expr; PUSHF $f: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Pushf64($f))
    };
    ($inst: expr; POP) => {
        $inst.push($crate::flame::instruction::Instruction::Pop)
    };
    ($inst: expr; ADDF) => {
        $inst.push($crate::flame::instruction::Instruction::Addf)
    };
    ($inst: expr; ADDI) => {
        $inst.push($crate::flame::instruction::Instruction::Addi)
    };
    ($inst: expr; SUBF) => {
        $inst.push($crate::flame::instruction::Instruction::Subf)
    };
    ($inst: expr; SUBI) => {
        $inst.push($crate::flame::instruction::Instruction::Subi)
    };
    ($inst: expr; MULTF) => {
        $inst.push($crate::flame::instruction::Instruction::Multf)
    };
    ($inst: expr; MULTI) => {
        $inst.push($crate::flame::instruction::Instruction::Multi)
    };
    ($inst: expr; DIVF) => {
        $inst.push($crate::flame::instruction::Instruction::Divf)
    };
    ($inst: expr; DIVI) => {
        $inst.push($crate::flame::instruction::Instruction::Divi)
    };
    ($inst: expr; MODF) => {
        $inst.push($crate::flame::instruction::Instruction::Modf)
    };
    ($inst: expr; MODI) => {
        $inst.push($crate::flame::instruction::Instruction::Modi)
    };
    ($inst: expr; NEGF) => {
        $inst.push($crate::flame::instruction::Instruction::Negf)
    };
    ($inst: expr; NEGI) => {
        $inst.push($crate::flame::instruction::Instruction::Negi)
    };
    ($inst: expr; FCOMP_LT) => {
        $inst.push($crate::flame::instruction::Instruction::FCompareLt)
    };
    ($inst: expr; ICOMP_LT) => {
        $inst.push($crate::flame::instruction::Instruction::ICompareLt)
    };
    ($inst: expr; FCOMP_LE) => {
        $inst.push($crate::flame::instruction::Instruction::FCompareLe)
    };
    ($inst: expr; ICOMP_LE) => {
        $inst.push($crate::flame::instruction::Instruction::ICompareLe)
    };
    ($inst: expr; FCOMP_GT) => {
        $inst.push($crate::flame::instruction::Instruction::FCompareGt)
    };
    ($inst: expr; ICOMP_GT) => {
        $inst.push($crate::flame::instruction::Instruction::ICompareGt)
    };
    ($inst: expr; FCOMP_GE) => {
        $inst.push($crate::flame::instruction::Instruction::FCompareGe)
    };
    ($inst: expr; ICOMP_GE) => {
        $inst.push($crate::flame::instruction::Instruction::ICompareGe)
    };
    ($inst: expr; FCOMP_EQ) => {
        $inst.push($crate::flame::instruction::Instruction::FCompareEq)
    };
    ($inst: expr; ICOMP_EQ) => {
        $inst.push($crate::flame::instruction::Instruction::ICompareEq)
    };
    ($inst: expr; NOT) => {
        $inst.push($crate::flame::instruction::Instruction::Not)
    };
    ($inst: expr; ALLOC_STR $str: expr) => {
        $inst.push($crate::flame::instruction::Instruction::AllocString($str))
    };
    ($inst: expr; STORE_LOCAL $i: expr) => {
        $inst.push($crate::flame::instruction::Instruction::StoreLocal($i))
    };
    ($inst: expr; LOAD_LOCAL $i: expr) => {
        $inst.push($crate::flame::instruction::Instruction::LoadLocal($i))
    };
    ($inst: expr; RET) => {
        $inst.push($crate::flame::instruction::Instruction::Return)
    };
    ($inst: expr; INV $i: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Invoke($i))
    };
    ($inst: expr; GOTO $j: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Goto($j))
    };
    ($inst: expr; GOTO_IF_TRUE $j: expr) => {
        $inst.push($crate::flame::instruction::Instruction::GotoIfTrue($j))
    };
    ($inst: expr; GOTO_IF_FALSE $j: expr) => {
        $inst.push($crate::flame::instruction::Instruction::GotoIfFalse($j))
    };
    ($inst: expr; POP_GOTO_IF_TRUE $j: expr) => {
        $inst.push($crate::flame::instruction::Instruction::PopGotoIfTrue($j))
    };
    ($inst: expr; POP_GOTO_IF_FALSE $j: expr) => {
        $inst.push($crate::flame::instruction::Instruction::PopGotoIfFalse($j))
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
    Pushu64(u64),
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

    AllocString(String),

    StoreLocal(usize),
    LoadLocal(usize),

    Return,

    Invoke(usize),

    Goto(usize),
    GotoIfTrue(usize),
    GotoIfFalse(usize),
    PopGotoIfTrue(usize),
    PopGotoIfFalse(usize),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pushu64(u64) => write!(f, "{:20}{u64}", "PUSHU"),
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
            Self::AllocString(str) => write!(f, "{:20}{str:?}", "ALLOC_STR"),
            Self::StoreLocal(index) => write!(f, "{:20}{index}", "STORE_LOCAL"),
            Self::LoadLocal(index) => write!(f, "{:20}{index}", "LOAD_LOCAL"),
            Self::Return => write!(f, "RET"),
            Self::Invoke(num_args) => write!(f, "{:20}{num_args}", "INV"),
            Self::Goto(index) => write!(f, "{:20}{index}", "GOTO"),
            Self::GotoIfTrue(index) => write!(f, "{:20}{index}", "GOTO_IF_TRUE"),
            Self::GotoIfFalse(index) => write!(f, "{:20}{index}", "GOTO_IF_FALSE"),
            Self::PopGotoIfTrue(index) => write!(f, "{:20}{index}", "POP_GOTO_IF_TRUE"),
            Self::PopGotoIfFalse(index) => write!(f, "{:20}{index}", "POP_GOTO_IF_FALSE"),
        }
    }
}

