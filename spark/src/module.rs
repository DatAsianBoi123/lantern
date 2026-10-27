use std::collections::HashMap;

use diagnostic::{FileId, Span, symbol::Symbol};
use parse::{Item, ItemFun, LanternFile};

use crate::{def::LanternFunction, scope::Scope, ty::{TypeContext, TypeId}};

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ModuleRegistry<'t> {
    modules: HashMap<FileId, Module<'t>>,
    order: Vec<FileId>,
}

impl<'t> ModuleRegistry<'t> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn len(&self) -> usize {
        self.order.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn index(&self, i: usize) -> Option<FileId> {
        self.order.get(i).copied()
    }

    pub fn get(&self, file: FileId) -> Option<&Module<'t>> {
        self.modules.get(&file)
    }

    pub fn get_mut(&mut self, file: FileId) -> Option<&mut Module<'t>> {
        self.modules.get_mut(&file)
    }

    pub fn insert(&mut self, module: Module<'t>) {
        self.order.push(module.file.source);
        self.modules.insert(module.file.source, module);
    }

    pub fn put_back(&mut self, module: Module<'t>) {
        self.modules.insert(module.file.source, module);
    }

    #[must_use]
    pub fn remove(&mut self, file: FileId) -> Option<Module<'t>> {
        self.modules.remove(&file)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Module<'t> {
    pub file: LanternFile,
    pub scope: Scope<'t, 't>,
    pub deps: Vec<ModuleDependency>,
    pub items: Vec<Item>,
    pub exports: ModuleExports<'t>,
    pub pending_bodies: Vec<PendingBody<'t>>,
}

impl<'t> Module<'t> {
    pub fn new(file: LanternFile, deps: Vec<ModuleDependency>, tcx: &TypeContext<'t>) -> Self {
        Self {
            file,
            scope: Scope::new_module(tcx),
            deps,
            items: Vec::new(),
            exports: ModuleExports::default(),
            pending_bodies: Vec::new(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleDependency {
    pub file: FileId,
    pub import: ModuleImport,
}

impl ModuleDependency {
    pub fn new(file: FileId, import: ModuleImport) -> Self {
        Self { file, import }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleImport {
    Glob(Span),
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct ModuleExports<'t> {
    pub types: TypeExports<'t>,
    pub funs: FunctionExports<'t>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct TypeExports<'t> {
    pub types: HashMap<Symbol, TypeId<'t>>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct FunctionExports<'t> {
    pub funs: HashMap<Symbol, LanternFunction<'t>>,
    pub associated: HashMap<TypeId<'t>, HashMap<Symbol, LanternFunction<'t>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PendingBody<'t> {
    pub fun: LanternFunction<'t>,
    pub def: ItemFun,
}

impl<'t> PendingBody<'t> {
    pub fn new(fun: LanternFunction<'t>, def: ItemFun) -> Self {
        Self { fun, def }
    }
}

