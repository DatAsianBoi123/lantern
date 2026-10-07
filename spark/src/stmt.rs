use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt<'t> {
    If(IfStmt<'t>),
    Match {
        expr: Expr<'t>,
        some_local: usize,
        some_arm: Vec<Stmt<'t>>,
        none_arm: Vec<Stmt<'t>>,
    },
    While {
        cond: Expr<'t>,
        stmts: Vec<Stmt<'t>>,
    },
    Val(usize, Option<Expr<'t>>),
    Return(Option<Expr<'t>>),
    Continue,
    Break,
    Throw(Expr<'t>),
    Expr(Expr<'t>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt<'t> {
    pub cond: Expr<'t>,
    pub stmts: Vec<Stmt<'t>>,
    pub branch: Option<IfBranch<'t>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBranch<'t> {
    ElseIf(Box<IfStmt<'t>>),
    Else(Vec<Stmt<'t>>),
}

