use std::fmt::{Display, Formatter};

use macros::Parse;

use crate::{Ident, Literal, Number, ParseTokens, QuotedString, Stmt, error::Span, punct::{And, Asterisk, Bang, ClosedBrace, ClosedParen, Comma, EqualsEquals, Greater, GreaterEq, Hyphen, Less, LessEq, OpenBrace, OpenParen, Or, Percent, Period, Plus, Slash}, stream::{Punctuated, StreamBranch}};

#[derive(Parse, Debug, Clone, PartialEq)]
enum PrimaryExpr {
    Literal(Literal),
    Variable(Ident),
    Paren(ExprParen),
    Block(ExprBlock),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Variable(Ident),
    Field(ExprField),
    FunCall(ExprFunCall),
    Paren(ExprParen),
    Block(ExprBlock),
    Binary(ExprBinary),
    Unary(ExprUnary),
}

impl From<PrimaryExpr> for Expr {
    fn from(value: PrimaryExpr) -> Self {
        match value {
            PrimaryExpr::Literal(literal) => Self::Literal(literal),
            PrimaryExpr::Variable(ident) => Self::Variable(ident),
            PrimaryExpr::Paren(expr) => Self::Paren(expr),
            PrimaryExpr::Block(block) => Self::Block(block),
        }
    }
}

impl Expr {
    pub fn span(&self) -> &Span {
        // TODO: ended span
        match self {
            Expr::Literal(Literal::Number(Number(_, span))) => span,
            Expr::Literal(Literal::Boolean(bool)) => bool.span(),
            Expr::Literal(Literal::String(QuotedString(_, span))) => span,
            Expr::Variable(Ident(_, span)) => span,
            Expr::Field(ExprField { expr, .. }) => expr.span(),
            Expr::FunCall(ExprFunCall { expr, .. }) => expr.span(),
            Expr::Paren(ExprParen { open_paren, .. }) => &open_paren.0,
            Expr::Block(ExprBlock { open_brace, .. }) => &open_brace.0,
            Expr::Binary(ExprBinary { lhs, .. }) => lhs.span(),
            Expr::Unary(ExprUnary { op, .. }) => op.span(),
        }
    }

    fn parse_all(stream: &mut StreamBranch, min_bp: u8) -> crate::Result<Self> {
        let mut lhs = Self::parse_lhs(stream)?;
        loop {
            if <Option<Period>>::parse(stream)?.is_some() {
                lhs = Self::Field(ExprField { expr: Box::new(lhs), ident: Ident::parse(stream)? });
                continue;
            }

            let before = stream.location();

            if let Ok(op) = BinaryOperator::parse(stream) {
                let (left_bp, right_bp) = op.binding_power();

                if left_bp < min_bp {
                    stream.goto(before);
                    break;
                }

                let rhs = Self::parse_all(stream, right_bp)?;
                lhs = Self::Binary(ExprBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) });

                continue;
            }

            if let Ok(open_paren) = OpenParen::parse(stream) {
                // highest BP

                lhs = Self::FunCall(ExprFunCall { expr: Box::new(lhs), open_paren, args: ParseTokens::parse(stream)?, closed_paren: ClosedParen::parse(stream)? });

                continue;
            }

            stream.goto(before);
            break;
        };
        Ok(lhs)
    }

    fn parse_lhs(stream: &mut StreamBranch) -> crate::Result<Self> {
        match <Option<UnaryOperator>>::parse(stream)? {
            Some(op) => {
                let right_bp = op.right_binding_power();
                let rhs = Self::parse_all(stream, right_bp)?;
                Ok(Self::Unary(ExprUnary { op, expr: Box::new(rhs) }))
            },
            None => Ok(PrimaryExpr::parse(stream)?.into()),
        }
    }
}

impl ParseTokens for Expr {
    fn parse(stream: &mut StreamBranch) -> crate::Result<Self> {
        Self::parse_all(stream, 0)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprField {
    pub expr: Box<Expr>,
    pub ident: Ident,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprFunCall {
    pub expr: Box<Expr>,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, Expr, Comma>,
    pub closed_paren: ClosedParen,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprMethodCall {
    pub expr: Box<Expr>,
    pub ident: Ident,
    pub open_paren: OpenParen,
    pub args: Vec<Expr>,
    pub closed_paren: ClosedParen,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprParen {
    pub open_paren: OpenParen,
    pub expr: Box<Expr>,
    pub closed_paren: ClosedParen,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub open_brace: OpenBrace,
    pub stmts: Vec<Stmt>,
    pub closed_brace: ClosedBrace,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub lhs: Box<Expr>,
    pub op: BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Add(Plus),
    Sub(Hyphen),
    Mult(Asterisk),
    Div(Slash),
    Mod(Percent),

    Eq(EqualsEquals),
    Le(LessEq),
    Ge(GreaterEq),
    Lt(Less),
    Gt(Greater),

    And(And),
    Or(Or),
}

impl BinaryOperator {
    pub fn span(&self) -> &Span {
        match self {
            Self::Add(punct) => &punct.0,
            Self::Sub(punct) => &punct.0,
            Self::Mult(punct) => &punct.0,
            Self::Div(punct) => &punct.0,
            Self::Mod(punct) => &punct.0,

            Self::Lt(punct) => &punct.0,
            Self::Le(punct) => &punct.0,
            Self::Gt(punct) => &punct.0,
            Self::Ge(punct) => &punct.0,
            Self::Eq(punct) => &punct.0,

            Self::And(punct) => &punct.0,
            Self::Or(punct) => &punct.0,
        }
    }

    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Mult(_) | Self::Div(_) | Self::Mod(_) => (9, 10),
            Self::Add(_) | Self::Sub(_) => (7, 8),
            Self::Lt(_) | Self::Le(_) | Self::Gt(_) | Self::Ge(_) | Self::Eq(_) => (5, 6),
            Self::And(_) => (3, 4),
            Self::Or(_) => (1, 2),
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

            Self::Lt(punct) => punct.fmt(f),
            Self::Le(punct) => punct.fmt(f),
            Self::Gt(punct) => punct.fmt(f),
            Self::Ge(punct) => punct.fmt(f),
            Self::Eq(punct) => punct.fmt(f),

            Self::And(punct) => punct.fmt(f),
            Self::Or(punct) => punct.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprUnary {
    pub op: UnaryOperator,
    pub expr: Box<Expr>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate(Hyphen),
    Not(Bang),
}

impl UnaryOperator {
    pub fn span(&self) -> &Span {
        match self {
            Self::Negate(punct) => &punct.0,
            Self::Not(punct) => &punct.0,
        }
    }

    pub fn right_binding_power(&self) -> u8 {
        match self {
            Self::Negate(_) | Self::Not(_) => 11,
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

