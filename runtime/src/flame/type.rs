use std::{cell::RefCell, collections::HashSet, ops::Deref, ptr};

use arena::Arena;
use diagnostic::{Diagnostic, error, symbol::{SymbolDisplay, SymbolTable}};
use parse::{FunType, Type, lex::TokenKind};

use crate::flame::{LanternPrimitive, LanternStruct, scope::Scope};

#[derive(Debug, Clone, Copy, Hash)]
pub struct TypeId<'t>(&'t LanternType<'t>);

impl PartialEq for TypeId<'_> {
    fn eq(&self, other: &Self) -> bool {
        // avoid stack overflows with recursive data types
        ptr::eq(self.0, other.0)
    }
}

impl Eq for TypeId<'_> { }

impl SymbolDisplay for TypeId<'_> {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        self.0.display(symbol_table)
    }
}

impl<'t> Deref for TypeId<'t> {
    type Target = LanternType<'t>;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LanternType<'t> {
    Struct(LanternStruct<'t>),
    Primitive(&'static LanternPrimitive),
    Array(TypeId<'t>),
    Function {
        is_method: bool,
        args: Vec<TypeId<'t>>,
        ret: TypeId<'t>,
    },
    Null,
}

impl SymbolDisplay for LanternType<'_> {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        match self {
            Self::Struct(LanternStruct { name, .. }) => symbol_table.resolve(*name).to_string(),
            Self::Primitive(LanternPrimitive { name, .. }) => (*name).to_string(),
            Self::Array(id) => format!("[{}]", id.display(symbol_table)),
            Self::Function { args, ret, .. } => {
                format!("fun({}) -> {}", args.iter().map(|ty| ty.display(symbol_table)).collect::<Vec<_>>().join(", "), ret.display(symbol_table))
            }
            Self::Null => "null".to_string(),
        }
    }
}

impl<'t> LanternType<'t> {
    pub fn resolve(ty: &Type, scope: &Scope<'_, 't>, tcx: &TypeContext<'t>) -> Result<TypeId<'t>, Diagnostic> {
        let r#type = match ty {
            Type::Array(_, inner, _) => Self::Array(Self::resolve(inner, scope, tcx)?),
            Type::Fun(FunType { args, ret, .. }) => {
                let args = args.iter().map(|r#type| Self::resolve(r#type, scope, tcx)).collect::<Result<_, _>>()?;
                let ret = ret.as_ref()
                    .map(|(_, r#type)| Self::resolve(r#type, scope, tcx))
                    .unwrap_or(Ok(tcx.intern(Self::Null)))?;
                Self::Function { is_method: false, args, ret }
            }
            Type::Path(path) => {
                let span = path.items[0].span();
                return scope.item(path.last().0).ok_or(error!(span => "unknown type"));
            }
        };
        Ok(tcx.intern(r#type))
    }

    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_) | Self::Function { .. })
    }

    pub fn is_ref(&self) -> bool {
        matches!(self, Self::Struct(_) | Self::Array(..))
    }

    pub fn is_primitive_type(&self, primitive: &'static LanternPrimitive) -> bool {
        *self == Self::Primitive(primitive)
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Struct(_) => 8,
            Self::Primitive(LanternPrimitive { size, .. }) => *size,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            // null is a ptr
            Self::Null => 8,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Self::Struct(_) => 8,
            Self::Primitive(LanternPrimitive { align, .. }) => *align,
            Self::Array(..) => 8,
            Self::Function { .. } => 8,
            // null is a ptr
            Self::Null => 8,
        }
    }
}

#[derive(Debug)]
pub struct TypeContext<'t> {
    arena: &'t Arena<LanternType<'t>>,
    lookup: RefCell<HashSet<&'t LanternType<'t>>>,
}

impl<'t> TypeContext<'t> {
    pub fn new(arena: &'t Arena<LanternType<'t>>) -> Self {
        Self {
            arena,
            lookup: RefCell::new(HashSet::new()),
        }
    }

    pub fn null(&self) -> TypeId<'t> {
        self.intern(LanternType::Null)
    }

    pub fn primitive(&self, primitive: &'static LanternPrimitive) -> TypeId<'t> {
        self.intern(LanternType::Primitive(primitive))
    }

    pub fn intern(&self, ty: LanternType<'t>) -> TypeId<'t> {
        let mut lookup = self.lookup.borrow_mut();
        if let Some(id) = lookup.get(&ty) {
            TypeId(id)
        } else {
            let ty = self.arena.allocate(ty);
            lookup.insert(ty);
            TypeId(ty)
        }
    }
}

