use std::{fmt::{Display, Formatter}, ops::{Deref, DerefMut}};

#[macro_export]
macro_rules! inst {
    ($inst: expr; PUSHU $b: expr) => {
        $inst.push($crate::flame::instruction::Instruction::Pushu64($b))
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
    ($inst: expr; SUBF) => {
        $inst.push($crate::flame::instruction::Instruction::Subf)
    };
    ($inst: expr; MULTF) => {
        $inst.push($crate::flame::instruction::Instruction::Multf)
    };
    ($inst: expr; DIVF) => {
        $inst.push($crate::flame::instruction::Instruction::Divf)
    };
    ($inst: expr; MODF) => {
        $inst.push($crate::flame::instruction::Instruction::Modf)
    };
    ($inst: expr; NEGF) => {
        $inst.push($crate::flame::instruction::Instruction::Negf)
    };
    ($inst: expr; COMP_LT) => {
        $inst.push($crate::flame::instruction::Instruction::CompareLt)
    };
    ($inst: expr; COMP_LE) => {
        $inst.push($crate::flame::instruction::Instruction::CompareLe)
    };
    ($inst: expr; COMP_GT) => {
        $inst.push($crate::flame::instruction::Instruction::CompareGt)
    };
    ($inst: expr; COMP_GE) => {
        $inst.push($crate::flame::instruction::Instruction::CompareGe)
    };
    ($inst: expr; COMP_EQ) => {
        $inst.push($crate::flame::instruction::Instruction::CompareEq)
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
    ($inst: expr; LOAD_FUN $i: expr) => {
        $inst.push($crate::flame::instruction::Instruction::LoadFun($i))
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
    Pushf64(f64),
    Pop,
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

    CompareLt,
    CompareLe,
    CompareGt,
    CompareGe,
    CompareEq,
    /// POP     bool: value
    /// PUSH    bool: !value
    Not,

    AllocString(String),

    StoreLocal(usize),
    LoadLocal(usize),

    LoadFun(usize),

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
            Self::Pushf64(f64) => write!(f, "{:20}{f64}", "PUSHF"),
            Self::Pop => write!(f, "POP"),
            Self::Addf => write!(f, "ADDF"),
            Self::Subf => write!(f, "SUBF"),
            Self::Multf => write!(f, "MULTF"),
            Self::Divf => write!(f, "DIVF"),
            Self::Modf => write!(f, "MODF"),
            Self::Negf => write!(f, "NEGF"),
            Self::CompareLt => write!(f, "COMP_LT"),
            Self::CompareLe => write!(f, "COMP_LE"),
            Self::CompareGt => write!(f, "COMP_GT"),
            Self::CompareGe => write!(f, "COMP_GE"),
            Self::CompareEq => write!(f, "COMP_EQ"),
            Self::Not => write!(f, "NOT"),
            Self::AllocString(str) => write!(f, "{:20}{str:?}", "ALLOC_STR"),
            Self::StoreLocal(index) => write!(f, "{:20}{index}", "STORE_LOCAL"),
            Self::LoadLocal(index) => write!(f, "{:20}{index}", "LOAD_LOCAL"),
            Self::LoadFun(index) => write!(f, "{:20}{index}", "LOAD_FUN"),
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

