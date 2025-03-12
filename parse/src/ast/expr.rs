use std::iter::Peekable;

use crate::{lex::{Literal, TokenTree}, Parse, Result};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
}

impl Parse<TokenTree> for Expression {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        Ok(Self::Literal(Literal::parse(iter)?))
    }
}

