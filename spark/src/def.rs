use std::{cell::OnceCell, hash::{Hash, Hasher}};

use diagnostic::symbol::Symbol;
use parse::{Visibility, expr::{BinaryOperator, UnaryOperator}};

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
    pub args: Vec<(Symbol, TypeId<'t>)>,
    pub ret: TypeId<'t>,
    pub ty: TypeId<'t>,
}

impl<'t> LanternFunction<'t> {
    pub fn new(index: usize, args: Vec<(Symbol, TypeId<'t>)>, ret: TypeId<'t>, tcx: &TypeContext<'t>) -> Self {
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

    fn to_assoc_type(args: &[(Symbol, TypeId<'t>)], ret: TypeId<'t>) -> LanternType<'t> {
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

impl Hash for LanternStruct<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
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

    pub fn init(&self, fields: Box<[(Visibility, Symbol, TypeId<'t>)]>) {
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
    pub fn new(fields: Box<[(Visibility, Symbol, TypeId<'t>)]>) -> Self {
        let alignment = fields.iter()
            .map(|(_, _, ty)| ty.alignment())
            .max()
            .unwrap_or(1);

        let mut size = 0usize;
        let fields = fields.into_iter()
            .map(|(vis, name, ty)| {
                size = size.next_multiple_of(ty.alignment());
                let field = LanternStructField { vis, name, offset: size, ty };
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
    pub vis: Visibility,
    pub name: Symbol,
    pub offset: usize,
    pub ty: TypeId<'t>,
}

#[derive(Debug, Clone)]
pub struct LanternNative {
    pub name: &'static str,
    pub id: usize,
    pub repr: NativeRepr,
    pub ops: PrimitiveOps,
}

impl Hash for LanternNative {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.id);
    }
}

impl PartialEq for LanternNative {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for LanternNative { }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NativeRepr {
    Value {
        size: usize,
        align: usize,
    },
    Ref,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct PrimitiveOps {
    pub negate_op: Option<UnaryOperation>,
    pub not_op: Option<UnaryOperation>,
    pub add_op: Option<BinaryOperation>,
    pub sub_op: Option<BinaryOperation>,
    pub mult_op: Option<BinaryOperation>,
    pub div_op: Option<BinaryOperation>,
    pub mod_op: Option<BinaryOperation>,
    pub lt_op: Option<BinaryOperation>,
    pub le_op: Option<BinaryOperation>,
    pub ge_op: Option<BinaryOperation>,
    pub gt_op: Option<BinaryOperation>,
    pub eq_op: Option<BinaryOperation>,
    pub neq_op: Option<BinaryOperation>,
}

impl PrimitiveOps {
    pub fn new() -> Self {
        Self::default()
    }

    pub const fn none() -> Self {
        Self {
            negate_op: None,
            not_op: None,
            add_op: None,
            sub_op: None,
            mult_op: None,
            div_op: None,
            mod_op: None,
            lt_op: None,
            le_op: None,
            ge_op: None,
            gt_op: None,
            eq_op: None,
            neq_op: None,
        }
    }

    pub fn get_bin_op(&self, op: BinaryOperator) -> Option<BinaryOperation> {
        match op {
            BinaryOperator::Add(_) | BinaryOperator::AddAssign(_) => self.add_op,
            BinaryOperator::Sub(_) | BinaryOperator::SubAssign(_) => self.sub_op,
            BinaryOperator::Mult(_) | BinaryOperator::MultAssign(_) => self.mult_op,
            BinaryOperator::Div(_) | BinaryOperator::DivAssign(_) => self.div_op,
            BinaryOperator::Mod(_) | BinaryOperator::ModAssign(_) => self.mod_op,
            BinaryOperator::Lt(_) => self.lt_op,
            BinaryOperator::Le(_) => self.le_op,
            BinaryOperator::Gt(_) => self.gt_op,
            BinaryOperator::Ge(_) => self.ge_op,
            BinaryOperator::Eq(_) => self.eq_op,
            BinaryOperator::Neq(_) => self.neq_op,
            _ => None,
        }
    }

    pub fn get_un_op(&self, op: UnaryOperator) -> Option<UnaryOperation> {
        match op {
            UnaryOperator::Not(_) => self.not_op,
            UnaryOperator::Negate(_) => self.negate_op,
        }
    }
}

