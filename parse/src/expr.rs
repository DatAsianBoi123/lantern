use std::fmt::{Display, Formatter};

use macros::Parse;

use crate::{Ident, Literal, Stmt, error::Span, punct::{Asterisk, Bang, ClosedBrace, ClosedParen, Comma, Hyphen, OpenBrace, OpenParen, Percent, Plus, Slash, Spaced}, stream::{Punctuated, Repetition}};

#[derive(Parse, Debug, Clone, PartialEq)]
#[from(ExprRaw)]
pub enum Expr {
    Literal(Literal),
    Variable(Ident),
    FunCall(ExprFunCall),
    Paren(ExprParen),
    Block(ExprBlock),
    Binary(ExprBinary),
    Unary(ExprUnary),
}

impl From<ExprRaw> for Expr {
    fn from(value: ExprRaw) -> Self {
        let lhs = value.term.into();
        value.additional.0.into_iter()
            .fold(lhs, |acc, (op, term)| {
                let bin_op = match op {
                    AdditiveOperator::Add(punct) => BinaryOperator::Add(punct),
                    AdditiveOperator::Sub(punct) => BinaryOperator::Sub(punct),
                };

                Self::Binary(ExprBinary { lhs: Box::new(acc), op: bin_op, rhs: Box::new(term.into()) })
            })
    }
}

impl From<Term> for Expr {
    fn from(value: Term) -> Self {
        let lhs = value.factor.into();
        value.additional.0.into_iter()
            .fold(lhs, |acc, (op, factor)| {
                let bin_op = match op {
                    MultiplicativeOperator::Mult(punct) => BinaryOperator::Mult(punct),
                    MultiplicativeOperator::Div(punct) => BinaryOperator::Div(punct),
                    MultiplicativeOperator::Mod(punct) => BinaryOperator::Mod(punct),
                };

                Self::Binary(ExprBinary { lhs: Box::new(acc), op: bin_op, rhs: Box::new(factor.into()) })
            })
    }
}

impl From<Factor> for Expr {
    fn from(value: Factor) -> Self {
        match value {
            Factor::Literal(literal) => Self::Literal(literal),
            Factor::Unary(op, factor) => Self::Unary(ExprUnary { op, expr: Box::new((*factor).into()) }),
            Factor::Variable(ident) => Self::Variable(ident),
            Factor::FunCall(ident, open_paren, args, closed_paren) => Self::FunCall(ExprFunCall { ident, open_paren, args: args.0.into_iter().map(Into::into).collect(), closed_paren }),
            Factor::Paren(open_paren, expr, closed_paren) => Self::Paren(ExprParen { open_paren, expr: Box::new((*expr).into()), closed_paren }),
            Factor::Block(open_brace, stmts, closed_brace) => Self::Block(ExprBlock { open_brace, stmts, closed_brace }),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprFunCall {
    pub ident: Ident,
    pub open_paren: OpenParen<Spaced>,
    pub args: Vec<Expr>,
    pub closed_paren: ClosedParen<Spaced>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprParen {
    pub open_paren: OpenParen<Spaced>,
    pub expr: Box<Expr>,
    pub closed_paren: ClosedParen<Spaced>,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub open_brace: OpenBrace<Spaced>,
    pub stmts: Vec<Stmt>,
    pub closed_brace: ClosedBrace<Spaced>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub lhs: Box<Expr>,
    pub op: BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Add(Plus<Spaced>),
    Sub(Hyphen<Spaced>),
    Mult(Asterisk<Spaced>),
    Div(Slash<Spaced>),
    Mod(Percent<Spaced>),
}

impl BinaryOperator {
    pub fn span(&self) -> &Span {
        match self {
            Self::Add(punct) => &punct.0,
            Self::Sub(punct) => &punct.0,
            Self::Mult(punct) => &punct.0,
            Self::Div(punct) => &punct.0,
            Self::Mod(punct) => &punct.0,
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Add(punct) => punct.fmt(f),
            Self::Sub(punct) => punct.fmt(f),
            Self::Mult(punct) => punct.fmt(f),
            Self::Div(punct) => punct.fmt(f),
            Self::Mod(punct) => punct.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
enum AdditiveOperator {
    Add(Plus<Spaced>),
    Sub(Hyphen<Spaced>),
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
enum MultiplicativeOperator {
    Mult(Asterisk<Spaced>),
    Div(Slash<Spaced>),
    Mod(Percent<Spaced>),
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate(Hyphen<Spaced>),
    Not(Bang<Spaced>),
}

impl UnaryOperator {
    pub fn span(&self) -> &Span {
        match self {
            Self::Negate(punct) => &punct.0,
            Self::Not(punct) => &punct.0,
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negate(punct) => punct.fmt(f),
            Self::Not(punct) => punct.fmt(f),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
struct ExprRaw {
    term: Term,
    additional: Repetition<0, (AdditiveOperator, Term)>,
}

#[derive(Parse, Debug, Clone, PartialEq)]
struct Term {
    factor: Factor,
    additional: Repetition<0, (MultiplicativeOperator, Factor)>,
}

#[derive(Parse, Debug, Clone, PartialEq)]
enum Factor {
    Unary(UnaryOperator, Box<Factor>),
    Paren(OpenParen<Spaced>, Box<ExprRaw>, ClosedParen<Spaced>),
    Block(OpenBrace<Spaced>, Vec<Stmt>, ClosedBrace<Spaced>),
    Literal(Literal),
    FunCall(Ident, OpenParen<Spaced>, Punctuated<0, ExprRaw, Comma<Spaced>>, ClosedParen<Spaced>),
    Variable(Ident),
}

