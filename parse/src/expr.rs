use std::fmt::{Display, Formatter};

use diagnostic::{Span, error};
use lex::{And, Asterisk, Bang, ClosedBrace, ClosedBracket, ClosedParen, Colon, Comma, Equals, EqualsEquals, Greater, GreaterEq, Hyphen, Less, LessEq, Literal, OpenBrace, OpenBracket, OpenParen, Or, Percent, Period, Plus, Punct, Slash, Token, TokenKind};
use macros::Parse;

use crate::{Ident, ParseTokens, Result, Stmt, stream::{Punctuated, Repetition, TokenStream}};

#[derive(Debug, Clone, PartialEq)]
enum PrimaryExpr {
    Literal(Literal),
    Identifier(Ident),
    Struct(ExprStruct),
    Paren(ExprParen),
    Block(ExprBlock),
    Array(ExprArray),
}

impl ParseTokens for PrimaryExpr {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        match stream.peek()? {
            Token::Literal(_) => Ok(Self::Literal(Literal::parse(stream)?)),
            Token::Ident(_) => {
                // either Ident or Struct
                let Token::Ident(ident) = stream.next_token()? else { unreachable!() };
                if matches!(stream.peek()?, Token::Punct(Punct::OpenBrace(_))) {
                    // Struct
                    Ok(Self::Struct(ExprStruct {
                        ident,
                        open_brace: ParseTokens::parse(stream)?,
                        fields: ParseTokens::parse(stream)?,
                        closed_brace: ParseTokens::parse(stream)?,
                    }))
                } else {
                    // Ident
                    Ok(Self::Identifier(ident))
                }
            },
            Token::Punct(Punct::OpenParen(_)) => Ok(Self::Paren(ExprParen::parse(stream)?)),
            Token::Punct(Punct::OpenBrace(_)) => Ok(Self::Block(ExprBlock::parse(stream)?)),
            Token::Punct(Punct::OpenBracket(_)) => Ok(Self::Array(ExprArray::parse(stream)?)),
            token => Err(error!(token.span() => "expected `expr`")),
        }
    }

    fn can_parse(peek: &Token) -> bool {
        Literal::can_parse(peek) || Ident::can_parse(peek) || ExprParen::can_parse(peek) || ExprBlock::can_parse(peek) || ExprArray::can_parse(peek)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(Literal),
    Identifier(Ident),
    Field(ExprField),
    FunCall(ExprFunCall),
    Struct(ExprStruct),
    Paren(ExprParen),
    Block(ExprBlock),
    Array(ExprArray),
    Index(ExprIndex),
    Binary(ExprBinary),
    Unary(ExprUnary),
}

impl From<PrimaryExpr> for Expr {
    fn from(value: PrimaryExpr) -> Self {
        match value {
            PrimaryExpr::Literal(literal) => Self::Literal(literal),
            PrimaryExpr::Identifier(ident) => Self::Identifier(ident),
            PrimaryExpr::Struct(expr_struct) => Self::Struct(expr_struct),
            PrimaryExpr::Paren(expr) => Self::Paren(expr),
            PrimaryExpr::Block(block) => Self::Block(block),
            PrimaryExpr::Array(array) => Self::Array(array),
        }
    }
}

impl Expr {
    pub fn span(&self) -> Span {
        // TODO: ended span
        match self {
            Expr::Literal(literal) => literal.span(),
            Expr::Identifier(Ident(_, span)) => span.clone(),
            Expr::Field(ExprField { expr, .. }) => expr.span(),
            Expr::FunCall(ExprFunCall { expr, .. }) => expr.span(),
            Expr::Struct(ExprStruct { ident, .. }) => ident.span(),
            Expr::Paren(ExprParen { open_paren, .. }) => open_paren.0.clone(),
            Expr::Block(ExprBlock { open_brace, .. }) => open_brace.0.clone(),
            Expr::Array(ExprArray { open_bracket, .. }) => open_bracket.0.clone(),
            Expr::Index(ExprIndex { expr, .. }) => expr.span(),
            Expr::Binary(ExprBinary { lhs, .. }) => lhs.span(),
            Expr::Unary(ExprUnary { op, .. }) => op.span(),
        }
    }

    fn parse_all(stream: &mut TokenStream, min_bp: u8) -> Result<Self> {
        let mut lhs = Self::parse_lhs(stream)?;
        loop {
            if Period::can_parse(stream.peek()?) {
                Period::parse(stream)?;
                lhs = Self::Field(ExprField { expr: Box::new(lhs), ident: Ident::parse(stream)? });
                continue;
            }

            if BinaryOperator::can_parse(stream.peek()?) {
                // TODO: no clone?
                let op = BinaryOperator::parse(&mut stream.clone())?;
                let (left_bp, right_bp) = op.binding_power();

                if left_bp < min_bp {
                    break;
                }
                // operators are all only 1 token
                stream.next_token()?;

                let rhs = Self::parse_all(stream, right_bp)?;
                lhs = Self::Binary(ExprBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) });

                continue;
            }

            if OpenParen::can_parse(stream.peek()?) {
                // highest BP
                let open_paren = OpenParen::parse(stream)?;

                lhs = Self::FunCall(ExprFunCall { expr: Box::new(lhs), open_paren, args: ParseTokens::parse(stream)?, closed_paren: ClosedParen::parse(stream)? });

                continue;
            }

            if OpenBracket::can_parse(stream.peek()?) {
                // highest BP
                let open_bracket = OpenBracket::parse(stream)?;

                lhs = Self::Index(ExprIndex { expr: Box::new(lhs), open_bracket, index: Box::new(Expr::parse(stream)?), closed_bracket: ClosedBracket::parse(stream)? });

                continue;
            }

            break;
        };
        Ok(lhs)
    }

    fn parse_lhs(stream: &mut TokenStream) -> Result<Self> {
        if UnaryOperator::can_parse(stream.peek()?) {
            let op = UnaryOperator::parse(stream)?;
            let rhs = Self::parse_all(stream, op.right_binding_power())?;
            Ok(Self::Unary(ExprUnary { op, expr: Box::new(rhs) }))
        } else {
            PrimaryExpr::parse(stream).map(Into::into)
        }
    }
}

impl ParseTokens for Expr {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        Self::parse_all(stream, 0)
    }

    fn can_parse(peek: &Token) -> bool {
        UnaryOperator::can_parse(peek) || PrimaryExpr::can_parse(peek)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprField {
    pub expr: Box<Expr>,
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq)]
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
pub struct ExprStruct {
    pub ident: Ident,
    pub open_brace: OpenBrace,
    pub fields: Punctuated<0, ExprStructField, Comma>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprStructField {
    pub ident: Ident,
    pub colon: Colon,
    pub expr: Expr,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprParen {
    pub open_paren: OpenParen,
    #[parse(Box::new(Expr::parse(stream)?))]
    pub expr: Box<Expr>,
    pub closed_paren: ClosedParen,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub open_brace: OpenBrace,
    pub stmts: Repetition<0, Stmt>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprArray {
    pub open_bracket: OpenBracket,
    pub elements: Punctuated<0, Expr, Comma>,
    pub closed_bracket: ClosedBracket,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprIndex {
    pub expr: Box<Expr>,
    pub open_bracket: OpenBracket,
    pub index: Box<Expr>,
    pub closed_bracket: ClosedBracket,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprBinary {
    pub lhs: Box<Expr>,
    pub op: BinaryOperator,
    pub rhs: Box<Expr>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Assign(Equals),

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
    pub fn span(&self) -> Span {
        match self {
            Self::Assign(punct) => punct.span(),

            Self::Add(punct) => punct.span(),
            Self::Sub(punct) => punct.span(),
            Self::Mult(punct) => punct.span(),
            Self::Div(punct) => punct.span(),
            Self::Mod(punct) => punct.span(),

            Self::Lt(punct) => punct.span(),
            Self::Le(punct) => punct.span(),
            Self::Gt(punct) => punct.span(),
            Self::Ge(punct) => punct.span(),
            Self::Eq(punct) => punct.span(),

            Self::And(punct) => punct.span(),
            Self::Or(punct) => punct.span(),
        }
    }

    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Mult(_) | Self::Div(_) | Self::Mod(_) => (11, 12),
            Self::Add(_) | Self::Sub(_) => (9, 10),
            Self::Lt(_) | Self::Le(_) | Self::Gt(_) | Self::Ge(_) | Self::Eq(_) => (7, 8),
            Self::And(_) => (5, 6),
            Self::Or(_) => (3, 4),
            Self::Assign(_) => (2, 1),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assign(punct) => punct.fmt(f),

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
    pub fn span(&self) -> Span {
        match self {
            Self::Negate(punct) => punct.0.clone(),
            Self::Not(punct) => punct.0.clone(),
        }
    }

    pub fn right_binding_power(&self) -> u8 {
        match self {
            Self::Negate(_) | Self::Not(_) => 13,
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

