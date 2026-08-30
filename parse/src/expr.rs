use std::fmt::{Display, Formatter};

use diagnostic::{Span, error};
use lex::{And, Asterisk, AsteriskEq, Bang, ClosedBrace, ClosedBracket, ClosedParen, Colon, Comma, Equals, EqualsEquals, Greater, GreaterEq, Hyphen, HyphenEq, Less, LessEq, Literal, NotEquals, OpenBrace, OpenBracket, OpenParen, Or, Percent, PercentEq, Period, Plus, PlusEq, Punct, Slash, SlashEq, Token, TokenKind};
use macros::Parse;

use crate::{Ident, ParseTokens, Result, Stmt, Type, stream::{TokenStream, parse_punctuated, parse_repetition}};

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
            Token::Literal(_) => Ok(Self::Literal(stream.parse()?)),
            Token::Ident(_) => {
                // either Ident or Struct
                let Token::Ident(ident) = stream.next_token()? else { unreachable!() };
                match stream.peek()? {
                    // Struct
                    Token::Punct(Punct::OpenBrace(_)) => {
                        Ok(Self::Struct(ExprStruct {
                            ident,
                            open_brace: stream.parse()?,
                            fields: parse_punctuated::<ExprStructField, Comma, ClosedBrace>(stream)?,
                            closed_brace: stream.parse()?,
                        }))
                    }
                    // Ident
                    _ => Ok(Self::Identifier(ident)),
                }
            },
            Token::Punct(Punct::OpenParen(_)) => Ok(Self::Paren(stream.parse()?)),
            Token::Punct(Punct::OpenBrace(_)) => Ok(Self::Block(stream.parse()?)),
            Token::Punct(Punct::OpenBracket(_)) => Ok(Self::Array(stream.parse()?)),
            token => Err(error!(token.span() => "expected `expr`")),
        }
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
            if Period::is_token(stream.peek()?) {
                let _ = stream.next_token();
                lhs = Self::Field(ExprField { expr: Box::new(lhs), ident: stream.parse()? });
                continue;
            }

            if let Some((left_bp, right_bp)) = BinaryOperator::try_get_binding_power(stream.peek()?) {
                if left_bp < min_bp {
                    break;
                }
                let op = stream.parse()?;

                let rhs = Self::parse_all(stream, right_bp)?;
                lhs = Self::Binary(ExprBinary { lhs: Box::new(lhs), op, rhs: Box::new(rhs) });

                continue;
            }

            if OpenParen::is_token(stream.peek()?) {
                // highest BP
                let open_paren = stream.parse()?;

                lhs = Self::FunCall(ExprFunCall { expr: Box::new(lhs), open_paren, args: parse_punctuated::<Expr, Comma, ClosedParen>(stream)?, closed_paren: stream.parse()? });

                continue;
            }

            if OpenBracket::is_token(stream.peek()?) {
                // highest BP
                let open_bracket = stream.parse()?;

                lhs = Self::Index(ExprIndex { expr: Box::new(lhs), open_bracket, index: Box::new(stream.parse()?), closed_bracket: stream.parse()? });

                continue;
            }

            break;
        };
        Ok(lhs)
    }

    fn parse_lhs(stream: &mut TokenStream) -> Result<Self> {
        if UnaryOperator::is_token(stream.peek()?) {
            let op = stream.parse::<UnaryOperator>()?;
            let rhs = Self::parse_all(stream, op.right_binding_power())?;
            Ok(Self::Unary(ExprUnary { op, expr: Box::new(rhs) }))
        } else {
            stream.parse::<PrimaryExpr>().map(Into::into)
        }
    }
}

impl ParseTokens for Expr {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        Self::parse_all(stream, 0)
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
    pub args: Vec<Expr>,
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
    #[parse(with(parse_punctuated::<ExprStructField, Comma, ClosedBrace>))]
    pub fields: Vec<ExprStructField>,
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
    #[parse(boxed(Expr))]
    pub expr: Box<Expr>,
    pub closed_paren: ClosedParen,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ExprBlock {
    pub open_brace: OpenBrace,
    #[parse(with(parse_repetition::<Stmt, ClosedBrace>))]
    pub stmts: Vec<Stmt>,
    pub closed_brace: ClosedBrace,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExprArray {
    pub open_bracket: OpenBracket,
    pub elements: Vec<Expr>,
    pub closed_bracket: ClosedBracket,
    pub ty: Option<Type>,
}

impl ParseTokens for ExprArray {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let open_bracket = stream.parse()?;
        let elements = parse_punctuated::<_, Comma, ClosedBracket>(stream)?;
        let closed_bracket = stream.parse()?;

        let ty = match stream.peek()? {
            Token::Punct(Punct::OpenBracket(_)) => {
                let _ = stream.next_token();
                let ty = stream.parse()?;
                stream.parse::<ClosedBracket>()?;
                Some(ty)
            },
            _ => None,
        };

        Ok(Self {
            open_bracket,
            elements,
            closed_bracket,
            ty,
        })
    }
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

    AddAssign(PlusEq),
    SubAssign(HyphenEq),
    MultAssign(AsteriskEq),
    DivAssign(SlashEq),
    ModAssign(PercentEq),

    Neq(NotEquals),
    Eq(EqualsEquals),
    Le(LessEq),
    Ge(GreaterEq),
    Lt(Less),
    Gt(Greater),

    And(And),
    Or(Or),
}

impl BinaryOperator {
    pub fn try_get_binding_power(peek: &Token) -> Option<(u8, u8)> {
        macro_rules! Punct {
            ($id:ident $(| $id2:ident)*) => {
                Token::Punct(Punct::$id(_) $(| Punct::$id2(_))*)
            };
        }

        match peek {
            Punct!(Asterisk | Slash | Percent) => Some((11, 12)),
            Punct!(Plus | Hyphen) => Some((9, 10)),
            Punct!(Less | LessEq | Greater | GreaterEq | EqualsEquals | NotEquals) => Some((7, 8)),
            Punct!(And) => Some((5, 6)),
            Punct!(Or) => Some((3, 4)),
            Punct!(Equals | PlusEq | HyphenEq | AsteriskEq | SlashEq | PercentEq) => Some((2, 1)),
            _ => None,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Assign(punct) => punct.span(),

            Self::Add(punct) => punct.span(),
            Self::Sub(punct) => punct.span(),
            Self::Mult(punct) => punct.span(),
            Self::Div(punct) => punct.span(),
            Self::Mod(punct) => punct.span(),

            Self::AddAssign(punct) => punct.span(),
            Self::SubAssign(punct) => punct.span(),
            Self::MultAssign(punct) => punct.span(),
            Self::DivAssign(punct) => punct.span(),
            Self::ModAssign(punct) => punct.span(),

            Self::Lt(punct) => punct.span(),
            Self::Le(punct) => punct.span(),
            Self::Gt(punct) => punct.span(),
            Self::Ge(punct) => punct.span(),
            Self::Eq(punct) => punct.span(),
            Self::Neq(punct) => punct.span(),

            Self::And(punct) => punct.span(),
            Self::Or(punct) => punct.span(),
        }
    }

    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Mult(_) | Self::Div(_) | Self::Mod(_) => (11, 12),
            Self::Add(_) | Self::Sub(_) => (9, 10),
            Self::Lt(_) | Self::Le(_) | Self::Gt(_) | Self::Ge(_) | Self::Eq(_) | Self::Neq(_) => (7, 8),
            Self::And(_) => (5, 6),
            Self::Or(_) => (3, 4),
            Self::Assign(_)
            | Self::AddAssign(_)
            | Self::SubAssign(_)
            | Self::MultAssign(_)
            | Self::DivAssign(_)
            | Self::ModAssign(_) => (2, 1),
        }
    }

    pub fn is_comparison(&self) -> bool {
        matches!(self, Self::Lt(_) | Self::Le(_) | Self::Gt(_) | Self::Ge(_) | Self::Eq(_) | Self::Neq(_))
    }

    pub fn is_token(token: &Token) -> bool {
        matches!(token,
            Token::Punct(Punct::Equals(_)) |
            Token::Punct(Punct::Plus(_)) |
            Token::Punct(Punct::Hyphen(_)) |
            Token::Punct(Punct::Asterisk(_)) |
            Token::Punct(Punct::Slash(_)) |
            Token::Punct(Punct::Percent(_)) |
            Token::Punct(Punct::PlusEq(_)) |
            Token::Punct(Punct::HyphenEq(_)) |
            Token::Punct(Punct::AsteriskEq(_)) |
            Token::Punct(Punct::SlashEq(_)) |
            Token::Punct(Punct::PercentEq(_)) |
            Token::Punct(Punct::EqualsEquals(_)) |
            Token::Punct(Punct::NotEquals(_)) |
            Token::Punct(Punct::LessEq(_)) |
            Token::Punct(Punct::GreaterEq(_)) |
            Token::Punct(Punct::Less(_)) |
            Token::Punct(Punct::Greater(_)) |
            Token::Punct(Punct::And(_)) |
            Token::Punct(Punct::Or(_))
        )
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

            Self::AddAssign(punct) => punct.fmt(f),
            Self::SubAssign(punct) => punct.fmt(f),
            Self::MultAssign(punct) => punct.fmt(f),
            Self::DivAssign(punct) => punct.fmt(f),
            Self::ModAssign(punct) => punct.fmt(f),

            Self::Lt(punct) => punct.fmt(f),
            Self::Le(punct) => punct.fmt(f),
            Self::Gt(punct) => punct.fmt(f),
            Self::Ge(punct) => punct.fmt(f),
            Self::Eq(punct) => punct.fmt(f),
            Self::Neq(punct) => punct.fmt(f),

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

    pub fn is_token(token: &Token) -> bool {
        matches!(token, Token::Punct(Punct::Hyphen(_)) | Token::Punct(Punct::Bang(_)))
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

