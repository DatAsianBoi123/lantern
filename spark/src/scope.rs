use std::{cell::OnceCell, collections::HashMap};

use diagnostic::{Span, symbol::Symbol};

use crate::{SparkFunction, def::{LanternFunction, LanternStructData, LanternVariable}, ty::{TypeContext, TypeId}};

#[derive(Debug, Clone)]
pub struct Scope<'a, 't> {
    items: HashMap<Symbol, TypeId<'t>>,
    functions: HashMap<Symbol, LanternFunction<'t>>,
    variables: HashMap<Symbol, LanternVariable<'t>>,
    associated: HashMap<TypeId<'t>, HashMap<Symbol, LanternFunction<'t>>>,
    kind: ScopeKind<'a, 't>,
    pub in_loop: bool,
    pub expected_ret: TypeId<'t>,
    pub diverges: bool,
    // TODO: reusing locals makes GC collect garbage data
    pub next_local_index: usize,
    pub max_locals: usize,
}

impl<'a, 't> Scope<'a, 't> {
    pub fn new_module(tcx: &TypeContext<'t>) -> Self {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Module,
            in_loop: false,
            expected_ret: tcx.none(),
            diverges: false,
            next_local_index: 0,
            max_locals: 0,
        }
    }

    pub fn kind(&self) -> &ScopeKind<'a, 't> {
        &self.kind
    }

    pub fn item(&self, name: Symbol) -> Option<TypeId<'t>> {
        match self.kind {
            ScopeKind::Module => self.items.get(&name).copied(),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                self.items.get(&name)
                    .copied()
                    .or_else(|| parent.item(name))
            }
        }
    }

    pub fn insert_item(&mut self, name: Symbol, item: TypeId<'t>) -> Option<()> {
        if self.items.contains_key(&name) { return None; };
        self.items.insert(name, item);
        Some(())
    }

    pub fn function(&self, name: Symbol) -> Option<&LanternFunction<'t>> {
        match self.kind {
            ScopeKind::Module => self.functions.get(&name),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                self.functions.get(&name)
                    .or_else(|| parent.function(name))
            }
        }
    }

    pub fn insert_function(&mut self, name: Symbol, fun: LanternFunction<'t>) -> Option<()> {
        if self.functions.contains_key(&name) { return None; };
        self.functions.insert(name, fun);
        Some(())
    }

    pub fn variable(&self, name: Symbol) -> Option<LanternVariable<'t>> {
        match self.kind {
            ScopeKind::Module | ScopeKind::Function(..) => self.variables.get(&name).copied(),
            ScopeKind::Block(parent) => {
                self.variables.get(&name)
                    .copied()
                    .or_else(|| parent.variable(name))
            }
        }
    }

    pub fn insert_variable(&mut self, name: Symbol, ty: TypeId<'t>) -> Option<usize> {
        if self.variables.contains_key(&name) { return None; };
        self.variables.insert(name, LanternVariable::new(self.next_local_index, ty));
        self.next_local_index += 1;
        self.max_locals = self.max_locals.max(self.next_local_index);
        Some(self.next_local_index - 1)
    }

    pub fn associated(&self, ty: TypeId<'t>, name: Symbol) -> Option<&LanternFunction<'t>> {
        match self.kind {
            ScopeKind::Module => self.associated.get(&ty).and_then(|associated| associated.get(&name)),
            ScopeKind::Block(parent) | ScopeKind::Function(parent, _) => {
                self.associated.get(&ty).and_then(|type_associated| type_associated.get(&name))
                    .or_else(|| parent.associated(ty, name))
            }
        }
    }

    pub fn insert_associated(&mut self, ty: TypeId<'t>, name: Symbol, fun: LanternFunction<'t>) -> Option<()> {
        let type_associated = self.associated.entry(ty).or_default();
        if type_associated.contains_key(&name) { return None; };
        type_associated.insert(name, fun);
        Some(())
    }

    pub fn inherit(&mut self, behavior: ScopeBehavior) {
        if behavior.diverges {
            self.diverges = true;
        }
        self.inherit_locals(behavior);
    }

    pub fn inherit_locals(&mut self, behavior: ScopeBehavior) {
        self.max_locals = self.max_locals.max(behavior.locals_used);
    }

    pub fn into_behavior(self) -> ScopeBehavior {
        ScopeBehavior { diverges: self.diverges, locals_used: self.max_locals }
    }
}

impl<'a: 'b, 'b, 't> Scope<'a, 't> {
    pub fn child_block(&'a self) -> Scope<'b, 't> {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Block(self),
            in_loop: self.in_loop,
            expected_ret: self.expected_ret,
            diverges: false,
            next_local_index: self.next_local_index,
            max_locals: self.max_locals,
        }
    }

    pub fn child_function(&'a self, span: Span, expected_ret: TypeId<'t>) -> Scope<'b, 't> {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Function(self, span),
            in_loop: false,
            expected_ret,
            diverges: false,
            next_local_index: 0,
            max_locals: 0,
        }
    }

    pub fn child_loop(&'a self) -> Scope<'b, 't> {
        Self {
            items: HashMap::new(),
            functions: HashMap::new(),
            variables: HashMap::new(),
            associated: HashMap::new(),
            kind: ScopeKind::Block(self),
            in_loop: true,
            expected_ret: self.expected_ret,
            diverges: false,
            next_local_index: self.next_local_index,
            max_locals: self.max_locals,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ScopeKind<'a, 't> {
    Module,
    Function(&'a Scope<'a, 't>, Span),
    Block(&'a Scope<'a, 't>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeBehavior {
    pub diverges: bool,
    pub locals_used: usize,
}

impl ScopeBehavior {
    pub fn guaranteed_converges() -> Self {
        Self {
            diverges: false,
            locals_used: 0,
        }
    }

    pub fn combine_branch(self, other: Self) -> Self {
        Self {
            diverges: self.diverges && other.diverges,
            locals_used: self.locals_used.max(other.locals_used),
        }
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

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Globals<'t> {
    pub types: Vec<&'t OnceCell<LanternStructData<'t>>>,
    pub funs: Vec<SparkFunction<'t>>,
    pub vars: GlobalVariables,
}

impl Globals<'_> {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GlobalVariables {
    // PERF: don't store two clones of string
    map: HashMap<Box<str>, usize>,
    strs: Vec<Box<str>>,
}

impl GlobalVariables {
    pub fn new() -> Self {
        Self { map: HashMap::new(), strs: Vec::new() }
    }

    pub fn insert_str(&mut self, str: Box<str>) -> usize {
        *self.map.entry(str.clone())
            .or_insert_with(|| {
                let id = self.strs.len();
                self.strs.push(str);
                id
            })
    }

    pub fn into_strs(self) -> Vec<Box<str>> {
        self.strs
    }
}

impl Default for GlobalVariables {
    fn default() -> Self {
        Self::new()
    }
}

