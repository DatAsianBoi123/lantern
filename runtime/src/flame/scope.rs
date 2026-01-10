use std::{collections::{HashMap, hash_map}};

use crate::flame::{GeneratedFunction, LanternFunction, LanternVariable, instruction::InstructionSet, r#type::LanternType};

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    functions: HashMap<String, LanternFunction>,
    variables: HashMap<String, LanternVariable>,
    kind: ScopeKind<'a>,
}

impl<'a> Default for Scope<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Scope<'a> {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
            kind: ScopeKind::Module,
        }
    }

    pub fn kind(&self) -> &ScopeKind<'_> {
        &self.kind
    }

    pub fn functions(&self) -> hash_map::Values<'_, String, LanternFunction> {
        self.functions.values()
    }

    pub fn function(&self, name: &str) -> Option<&LanternFunction> {
        match self.kind {
            ScopeKind::Module => self.functions.get(name),
            ScopeKind::Block(parent) | ScopeKind::Function(parent) => {
                self.functions.get(name)
                    .or_else(|| parent.function(name))
            }
        }
    }

    pub fn insert_function(&mut self, name: String, fun: LanternFunction) -> Option<()> {
        if self.functions.contains_key(&name) { return None; };
        self.functions.insert(name, fun);
        Some(())
    }

    pub fn variable(&self, name: &str) -> Option<LanternVariable> {
        match self.kind {
            ScopeKind::Module | ScopeKind::Function(_) => self.variables.get(name).cloned(),
            ScopeKind::Block(parent) => {
                self.variables.get(name).cloned()
                    .or_else(|| parent.variable(name))
            }
        }
    }

    pub fn insert_variable(&mut self, name: String, r#type: LanternType) -> Option<()> {
        if self.variables.contains_key(&name) { return None; };
        self.variables.insert(name, LanternVariable::new(r#type));
        Some(())
    }
}

impl<'a: 'b, 'b> Scope<'a> {
    pub fn child_block(&'a self) -> Scope<'b> {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
            kind: ScopeKind::Block(self),
        }
    }

    pub fn child_function(&'a self) -> Scope<'b> {
        Self {
            functions: HashMap::new(),
            variables: HashMap::new(),
            kind: ScopeKind::Function(self),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScopeKind<'a> {
    Module,
    Function(&'a Scope<'a>),
    Block(&'a Scope<'a>),
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub instructions: InstructionSet,
    locals: Vec<String>,
    pub ret_type: Option<LanternType>,
}

impl StackFrame {
    pub fn new_module() -> Self {
        Self {
            instructions: InstructionSet::new(),
            locals: Vec::new(),
            ret_type: None,
        }
    }

    pub fn new_fun(ret_type: LanternType) -> Self {
        Self {
            instructions: InstructionSet::new(),
            locals: Vec::new(),
            ret_type: Some(ret_type),
        }
    }

    pub fn declare_local(&mut self, name: String) -> usize {
        let index = self.locals.len();
        self.locals.push(name);
        index
    }

    pub fn find_local(&self, name: &str) -> Option<usize> {
        self.locals.iter()
            .enumerate()
            .find_map(|(i, var)| (var == name).then_some(i))
    }

    pub fn into_gen(self) -> GeneratedFunction {
        GeneratedFunction::Instructions(self.instructions)
    }
}

