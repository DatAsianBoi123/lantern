use std::collections::HashMap;

use crate::{instruction::InstructionSet, r#type::LanternType, Address};

#[derive(Debug, Clone, PartialEq)]
pub struct StackFrame<'a> {
    parent: Option<&'a StackFrame<'a>>,
    functions: HashMap<String, LanternFunction>,
    variables: HashMap<String, Address>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFunction {
    pub args: Vec<LanternType>,
    pub kind: LanternFunctionKind,
}

impl LanternFunction {
    pub fn new(args: Vec<LanternType>, kind: LanternFunctionKind) -> Self {
        Self { args, kind }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LanternFunctionKind {
    Native(u32),
    Custom(InstructionSet),
}

impl<'a> Default for StackFrame<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> StackFrame<'a> {
    pub fn new() -> Self {
        Self {
            parent: None,
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }

    pub fn function(&self, name: &str) -> Option<&LanternFunction> {
        self.functions.get(name)
            .or_else(|| self.parent.and_then(|stack_frame| stack_frame.function(name)))
    }

    pub fn insert_function(&mut self, name: String, fun: LanternFunction) {
        self.functions.insert(name, fun);
    }

    pub fn variable(&self, name: &str) -> Option<Address> {
        self.variables.get(name).copied()
            .or_else(|| self.parent.and_then(|stack_frame| stack_frame.variable(name)))
    }

    pub fn insert_variable(&mut self, name: String, addr: Address) {
        self.variables.insert(name, addr);
    }
}

impl<'a: 'b, 'b> StackFrame<'a> {
    pub fn child(&'a self) -> StackFrame<'b> {
        Self {
            parent: Some(self),
            functions: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

