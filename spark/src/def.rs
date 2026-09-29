use std::{cell::OnceCell, fmt::Formatter, hash};

use diagnostic::symbol::Symbol;
use parse::{expr::{BinaryOperator, UnaryOperator}, lex::Ident};

use crate::{expr::{BinaryOperation, UnaryOperation}, ty::{LanternType, TypeContext, TypeId}};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LanternVariable<'t> {
    pub index: usize,
    pub ty: TypeId<'t>,
}

impl<'t> LanternVariable<'t> {
    pub fn new(index: usize, ty: TypeId<'t>) -> Self {
        Self { index, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternFunction<'t> {
    pub index: usize,
    pub args: Vec<(Ident, TypeId<'t>)>,
    pub ret: TypeId<'t>,
    pub ty: TypeId<'t>,
}

impl<'t> LanternFunction<'t> {
    pub fn new(index: usize, args: Vec<(Ident, TypeId<'t>)>, ret: TypeId<'t>, tcx: &TypeContext<'t>) -> Self {
        Self {
            ty: tcx.intern(Self::to_assoc_type(&args, ret)),
            index,
            args,
            ret,
        }
    }

    pub fn has_receiver(&self, recv: TypeId<'t>) -> bool {
        self.args.first().is_some_and(|(_, ty)| *ty == recv)
    }

    fn to_assoc_type(args: &[(Ident, TypeId<'t>)], ret: TypeId<'t>) -> LanternType<'t> {
        LanternType::Function { args: args.iter().map(|(_, ty)| *ty).collect(), ret }
    }
}

#[derive(Debug, Clone)]
pub struct LanternStruct<'t> {
    pub name: Symbol,
    pub id: usize,
    pub data: OnceCell<LanternStructData<'t>>,
}

impl PartialEq for LanternStruct<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LanternStruct<'_> { }

impl hash::Hash for LanternStruct<'_> {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.id);
    }
}

impl<'t> LanternStruct<'t> {
    pub fn new(name: Symbol, index: usize) -> Self {
        Self {
            name,
            id: index,
            data: OnceCell::new(),
        }
    }

    pub fn init(&self, fields: Box<[(Symbol, TypeId<'t>)]>) {
        if self.data.set(LanternStructData::new(fields)).is_err() {
            panic!("double-init on lantern struct")
        }
    }

    pub fn data(&self) -> &LanternStructData<'t> {
        match self.data.get() {
            Some(data) => data,
            None => panic!("struct data not initialized"),
        }
    }

    pub fn find_field(&self, name: Symbol) -> Option<LanternStructField<'t>> {
        self.data().find_field(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LanternStructData<'t> {
    fields: Box<[LanternStructField<'t>]>,
    size: usize,
}

impl<'t> LanternStructData<'t> {
    pub fn new(fields: Box<[(Symbol, TypeId<'t>)]>) -> Self {
        let alignment = fields.iter()
            .map(|(_, ty)| ty.alignment())
            .max()
            .unwrap_or(1);

        let mut size = 0usize;
        let fields = fields.into_iter()
            .map(|(name, ty)| {
                size = size.next_multiple_of(ty.alignment());
                let field = LanternStructField { name, offset: size, ty };
                size += ty.size();
                field
            })
            .collect();
        size = size.next_multiple_of(alignment);

        Self {
            fields,
            size,
        }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn fields(&self) -> &[LanternStructField<'t>] {
        &self.fields
    }

    pub fn find_field(&self, name: Symbol) -> Option<LanternStructField<'t>> {
        self.fields.iter().find(|field| field.name == name).copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LanternStructField<'t> {
    pub name: Symbol,
    pub offset: usize,
    pub ty: TypeId<'t>,
}

#[derive(Clone)]
pub struct LanternPrimitive {
    pub name: &'static str,
    pub id: usize,
    pub size: usize,
    pub align: usize,
    pub ops: PrimitiveOps,
}

impl hash::Hash for LanternPrimitive {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.id);
    }
}

impl std::fmt::Debug for LanternPrimitive {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LanternPrimitive")
            .field("name", &self.name)
            .field("id", &self.id)
            .field("size", &self.size)
            .field("align", &self.align)
            .finish_non_exhaustive()
    }
}

impl PartialEq for LanternPrimitive {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LanternPrimitive { }

#[derive(Default, Debug, Clone, PartialEq)]
pub struct PrimitiveOps {
    pub negate_inst: Option<UnaryOperation>,
    pub not_inst: Option<UnaryOperation>,
    pub add_inst: Option<BinaryOperation>,
    pub sub_inst: Option<BinaryOperation>,
    pub mult_inst: Option<BinaryOperation>,
    pub div_inst: Option<BinaryOperation>,
    pub mod_inst: Option<BinaryOperation>,
    pub lt_inst: Option<BinaryOperation>,
    pub le_inst: Option<BinaryOperation>,
    pub ge_inst: Option<BinaryOperation>,
    pub gt_inst: Option<BinaryOperation>,
    pub eq_inst: Option<BinaryOperation>,
    pub neq_inst: Option<BinaryOperation>,
}

impl PrimitiveOps {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_bin_op(&self, op: BinaryOperator) -> Option<BinaryOperation> {
        match op {
            BinaryOperator::Add(_) | BinaryOperator::AddAssign(_) => self.add_inst,
            BinaryOperator::Sub(_) | BinaryOperator::SubAssign(_) => self.sub_inst,
            BinaryOperator::Mult(_) | BinaryOperator::MultAssign(_) => self.mult_inst,
            BinaryOperator::Div(_) | BinaryOperator::DivAssign(_) => self.div_inst,
            BinaryOperator::Mod(_) | BinaryOperator::ModAssign(_) => self.mod_inst,
            BinaryOperator::Lt(_) => self.lt_inst,
            BinaryOperator::Le(_) => self.le_inst,
            BinaryOperator::Gt(_) => self.gt_inst,
            BinaryOperator::Ge(_) => self.ge_inst,
            BinaryOperator::Eq(_) => self.eq_inst,
            BinaryOperator::Neq(_) => self.neq_inst,
            _ => None,
        }
    }

    pub fn get_un_op(&self, op: UnaryOperator) -> Option<UnaryOperation> {
        match op {
            UnaryOperator::Not(_) => self.not_inst,
            UnaryOperator::Negate(_) => self.negate_inst,
        }
    }
}

