use crate::{flame::{FunctionKind, GeneratedFunction, instruction::InstructionSet}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopContext {
    pub scopes: Vec<LoopScope>,
}

impl Default for LoopContext {
    fn default() -> Self {
        Self::new()
    }
}

impl LoopContext {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LoopScope {
    pub head: usize,
    pub breaks: Vec<usize>,
}

impl LoopScope {
    pub fn new(head: usize) -> Self {
        Self {
            head,
            breaks: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub name: String,
    pub instructions: InstructionSet,
    pub locals: usize,
    pub loop_context: LoopContext,
    pub line_table: Vec<LineMap>,
}

impl StackFrame {
    pub fn new(name: String, locals: usize) -> Self {
        Self {
            name,
            instructions: InstructionSet::new(),
            locals,
            loop_context: LoopContext::new(),
            line_table: Vec::new(),
        }
    }

    pub fn new_module() -> Self {
        Self {
            name: "<module>".to_string(),
            instructions: InstructionSet::new(),
            locals: 0,
            loop_context: LoopContext::new(),
            line_table: Vec::new(),
        }
    }

    pub fn into_gen(self) -> GeneratedFunction {
        let mut fun = GeneratedFunction::new(self.name.into_boxed_str(), FunctionKind::Instructions(self.instructions, self.locals));
        fun.line_table = self.line_table;
        fun
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineMap {
    pub ip: usize,
    pub line: u32,
}

impl LineMap {
    pub fn new(ip: usize, line: u32) -> Self {
        Self { ip, line }
    }
}

