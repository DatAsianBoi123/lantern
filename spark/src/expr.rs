use diagnostic::Span;

use crate::{stmt::Stmt, ty::{TypeContext, TypeId}};

#[derive(Debug, Clone, PartialEq)]
pub struct Expr<'t> {
    pub kind: ExprKind<'t>,
    pub ty: TypeId<'t>,
    pub span: Span,
}

impl<'t> Expr<'t> {
    pub fn new(kind: ExprKind<'t>, ty: TypeId<'t>, span: Span) -> Self {
        Self {
            kind,
            ty,
            span,
        }
    }

    pub fn error(tcx: &TypeContext<'t>, span: Span) -> Self {
        Self {
            kind: ExprKind::Error,
            ty: tcx.error(),
            span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind<'t> {
    Literal(Literal),
    Static(usize),
    Global(usize),
    Local(usize),
    Block(Vec<Stmt<'t>>),
    Field(Box<Expr<'t>>, usize),
    Len(Box<Expr<'t>>),
    Index(Box<Expr<'t>>, Box<Expr<'t>>),
    Binary(Box<Expr<'t>>, BinaryOperation, Box<Expr<'t>>),
    BinaryAssign(Box<Expr<'t>>, BinaryOperation, Box<Expr<'t>>),
    Unary(UnaryOperation, Box<Expr<'t>>),
    Logical(Box<Expr<'t>>, LogicalOperation, Box<Expr<'t>>),
    Assign(Box<Expr<'t>>, Box<Expr<'t>>),
    Call(Box<Expr<'t>>, Vec<Expr<'t>>),
    CallMethod(Box<Expr<'t>>, usize, Vec<Expr<'t>>),
    Struct(usize, Vec<(Expr<'t>, usize)>),
    Array(TypeId<'t>, Vec<Expr<'t>>),
    Error,
}

impl ExprKind<'_> {
    pub fn is_place(&self) -> bool {
        matches!(self, Self::Local(_) | Self::Field(..) | Self::Index(_, _))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    True,
    False,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperation {
    Addf,
    Addi,
    Subf,
    Subi,
    Multf,
    Multi,
    Divf,
    Divi,
    Modf,
    Modi,

    FCompareLt,
    ICompareLt,
    FCompareLe,
    ICompareLe,
    FCompareGt,
    ICompareGt,
    FCompareGe,
    ICompareGe,
    FCompareEq,
    ICompareEq,
    FCompareNeq,
    ICompareNeq,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperation {
    Negf,
    Negi,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogicalOperation {
    And,
    Or,
}

