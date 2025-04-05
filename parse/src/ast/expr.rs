use std::iter::Peekable;

use crate::{diagnostic, error::Diagnostics, lex::{Delimiter, Group, Ident, Literal, Punct, TokenTree}, Parse, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Variable(Ident),
    BinaryAdd(Box<Expression>, Box<Expression>),
    BinarySub(Box<Expression>, Box<Expression>),
    BinaryMult(Box<Expression>, Box<Expression>),
    BinaryDiv(Box<Expression>, Box<Expression>),
    BinaryMod(Box<Expression>, Box<Expression>),
    UnaryNegate(Box<Expression>),
    UnaryNot(Box<Expression>),
}

impl Parse<TokenTree> for Expression {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let items = ExpressionItem::read(iter)?;
        Self::parse_items(items)
    }
}

impl Expression {
    pub fn parse_items(mut items: Vec<ExpressionItem>) -> Result<Self> {
        if items.len() == 1 { return Self::parse_one(items.remove(0)) };
        if items.len() == 2 {
            return match (items.remove(0), items.remove(0)) {
                (ExpressionItem::UnOp(UnaryOperator::Negate), item) => Ok(Self::UnaryNegate(Box::new(Self::parse_one(item)?))),
                (ExpressionItem::UnOp(UnaryOperator::Not), item) => Ok(Self::UnaryNot(Box::new(Self::parse_one(item)?))),
                _ => Err(diagnostic!("expected unary operator").into()),
            };
        };

        let (i, op) = items.iter()
            .enumerate()
            .filter_map(|(i, item)| match item {
                ExpressionItem::BinOp(op) => Some((i, *op)),
                _ => None,
            })
            .reduce(|acc, (i, op)| {
                if op.order() < acc.1.order() {
                    (i, op)
                } else {
                    acc
                }
            })
            .ok_or::<Diagnostics>(diagnostic!("missing operator").into())?;

        let mut right = items.split_off(i);
        right.remove(0);

        let lhs = Box::new(Self::parse_items(items)?);
        let rhs = Box::new(Self::parse_items(right)?);
        match op {
            BinaryOperator::Add => Ok(Self::BinaryAdd(lhs, rhs)),
            BinaryOperator::Sub => Ok(Self::BinarySub(lhs, rhs)),
            BinaryOperator::Mult => Ok(Self::BinaryMult(lhs, rhs)),
            BinaryOperator::Div => Ok(Self::BinaryDiv(lhs, rhs)),
            BinaryOperator::Mod => Ok(Self::BinaryMod(lhs, rhs)),
        }
    }

    pub fn parse_one(item: ExpressionItem) -> Result<Self> {
        match item {
            ExpressionItem::Group(items) => Self::parse_items(items),
            ExpressionItem::Literal(literal) => Ok(Self::Literal(literal)),
            ExpressionItem::Variable(ident) => Ok(Self::Variable(ident)),
            _ => Err(diagnostic!("expected expression value").into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionItem {
    Group(Vec<ExpressionItem>),
    Literal(Literal),
    Variable(Ident),
    BinOp(BinaryOperator),
    UnOp(UnaryOperator),
}

impl ExpressionItem {
    pub fn read<I>(iter: &mut Peekable<I>) -> Result<Vec<Self>>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let mut items = Vec::new();

        let mut prev_is_value = false;
        while let Some(token) = iter.peek() {
            match token {
                TokenTree::Group(Group { delimiter: Delimiter::Paren, .. }) => {
                    let Some(TokenTree::Group(Group { delimiter: Delimiter::Paren, tokens })) = iter.next() else { unreachable!() };
                    items.push(Self::Group(Self::read(&mut tokens.into_iter().peekable())?));
                    prev_is_value = true;
                },
                TokenTree::Literal(_) => {
                    let Some(TokenTree::Literal(literal)) = iter.next() else { unreachable!() };
                    items.push(Self::Literal(literal));
                    prev_is_value = true;
                },
                TokenTree::Ident(_) => {
                    let Some(TokenTree::Ident(ident)) = iter.next() else { unreachable!() };
                    items.push(Self::Variable(ident));
                    prev_is_value = true;
                },
                TokenTree::Punct(punct) => {
                    match (punct, prev_is_value) {
                        (Punct::Plus(_), true) => items.push(Self::BinOp(BinaryOperator::Add)),
                        (Punct::Hyphen(_), true) => items.push(Self::BinOp(BinaryOperator::Sub)),
                        (Punct::Asterisk(_), true) => items.push(Self::BinOp(BinaryOperator::Mult)),
                        (Punct::Slash(_), true) => items.push(Self::BinOp(BinaryOperator::Div)),
                        (Punct::Percent(_), true) => items.push(Self::BinOp(BinaryOperator::Mod)),

                        (Punct::Hyphen(_), false) => items.push(Self::UnOp(UnaryOperator::Negate)),
                        (Punct::Bang(_), false) => items.push(Self::UnOp(UnaryOperator::Not)),

                        _ => break,
                    };
                    prev_is_value = false;
                    iter.next();
                },
                _ => break,
            }
        };

        Ok(items)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
}

impl BinaryOperator {
    pub fn order(&self) -> u8 {
        match self {
            Self::Add | Self::Sub => 0,
            Self::Mult | Self::Div | Self::Mod => 1,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOperator {
    Negate,
    Not,
}

