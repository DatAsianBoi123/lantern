use std::collections::HashMap;

use diagnostic::Span;

use crate::{flame::{FunctionKind, GeneratedFunction, LanternFunction, LanternItem, LanternStruct, LanternVariable, instruction::InstructionSet, r#type::LanternType}, heap::TypeInfo};

#[derive(Debug, Clone)]
pub struct Scope<'a> {
    items: HashMap<String, LanternItem>,
    functions: HashMap<String, LanternFunction>,
    variables: HashMap<String, LanternVariable>,
    associated: HashMap<ItemIdentifier, HashMap<String, LanternFunction>>,
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
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Module,
        }
    }

    pub fn kind(&self) -> &ScopeKind<'a> {
        &self.kind
    }

    pub fn into_kind(self) -> ScopeKind<'a> {
        self.kind
    }

    pub fn find_struct(&self, type_id: usize) -> &LanternStruct {
        match self.kind {
            ScopeKind::Module => {
                for item in self.items.values() {
                    if let LanternItem::Struct(r#struct @ LanternStruct { id, .. }) = item && *id == type_id {
                        return r#struct
                    }
                }
                panic!("struct with type id {type_id} not found");
            },
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                for item in self.items.values() {
                    if let LanternItem::Struct(r#struct @ LanternStruct { id, .. }) = item && *id == type_id {
                        return r#struct
                    }
                }
                parent.find_struct(type_id)
            }
        }
    }

    pub fn find_struct_mut_in_scope(&mut self, type_id: usize) -> Option<&mut LanternStruct> {
        self.items.values_mut().find_map(|item| match item {
            LanternItem::Struct(r#struct) if r#struct.id == type_id => {
                Some(r#struct)
            },
            _ => None,
        })
    }

    pub fn item(&self, name: &str) -> Option<&LanternItem> {
        match self.kind {
            ScopeKind::Module => self.items.get(name),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                self.items.get(name)
                    .or_else(|| parent.item(name))
            }
        }
    }

    pub fn insert_item(&mut self, name: String, item: LanternItem) -> Option<()> {
        if self.items.contains_key(&name) { return None; };
        self.items.insert(name, item);
        Some(())
    }

    pub fn function(&self, name: &str) -> Option<&LanternFunction> {
        match self.kind {
            ScopeKind::Module => self.functions.get(name),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
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
            ScopeKind::Module | ScopeKind::Function(..) => self.variables.get(name).cloned(),
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

    pub fn associated(&self, id: ItemIdentifier, name: &str) -> Option<&LanternFunction> {
        match self.kind {
            ScopeKind::Module => self.associated.get(&id).and_then(|associated| associated.get(name)),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                self.associated.get(&id).and_then(|type_associated| type_associated.get(name))
                    .or_else(|| parent.associated(id, name))
            }
        }
    }

    pub fn insert_associated(&mut self, id: ItemIdentifier, name: String, fun: LanternFunction) -> Option<()> {
        let type_associated = self.associated.entry(id)
            .or_default();
        if type_associated.contains_key(&name) { return None; };
        type_associated.insert(name, fun);
        Some(())
    }
}

impl<'a: 'b, 'b> Scope<'a> {
    pub fn child_block(&'a self) -> Scope<'b> {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Block(self),
        }
    }

    pub fn child_function(&'a self, span: Span) -> Scope<'b> {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Function(self, span),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ItemIdentifier {
    Struct(usize),
    Primitive(usize),
}

#[derive(Debug, Clone)]
pub enum ScopeKind<'a> {
    Module,
    Function(&'a Scope<'a>, Span),
    Block(&'a Scope<'a>),
}

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

    pub fn in_loop(&self) -> bool {
        !self.scopes.is_empty()
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
    locals: Vec<String>,
    pub line_table: Vec<LineMap>,
    pub ret_type: Option<LanternType>,
}

impl StackFrame {
    pub fn new_module() -> Self {
        Self {
            name: "<module>".to_string(),
            instructions: InstructionSet::new(),
            locals: Vec::new(),
            line_table: Vec::new(),
            ret_type: None,
        }
    }

    pub fn new_fun(name: String, ret_type: LanternType) -> Self {
        Self {
            name,
            instructions: InstructionSet::new(),
            locals: Vec::new(),
            line_table: Vec::new(),
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
        let mut fun = GeneratedFunction::new(self.name, FunctionKind::Instructions(self.instructions, self.locals.len()));
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

#[derive(Debug, Clone)]
pub struct Globals {
    pub funs: Vec<GeneratedFunction>,
    pub types: Vec<TypeInfo>,
}

