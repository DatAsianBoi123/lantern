use std::iter::Peekable;

use token::Val;

use crate::{diagnostic, lex::{Colon, Equals, Ident, Literal, TokenTree}, Parse, Result};

pub mod token;

macro_rules! parse_impl {
    (for $ty: ty : $(#[$meta: meta])* $vis: vis struct $ident: ident { $($item_vis: vis $item_ident: ident : $item_ty: ty),* $(,)? }) => {
        $(#[$meta])*
        $vis struct $ident {
            $($item_vis $item_ident : $item_ty,)*
        }

        impl $crate::Parse<$ty> for $ident {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self>
            where I: std::iter::Iterator<Item = $ty> + std::clone::Clone
            {
                $(
                    let $item_ident = <$item_ty as $crate::Parse<$ty>>::parse(iter)?;
                )*
                Ok(Self { $($item_ident,)* })
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFile {
    statements: Vec<Statement>,
}

impl Parse<TokenTree> for LanternFile {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let mut statements = Vec::new();

        while let Some(peek) = iter.peek() {
            if matches!(peek, TokenTree::Newline) {
                iter.next();
                continue;
            }

            statements.push(Statement::parse(iter)?);

            if let Some(peek) = iter.peek() {
                if matches!(peek, TokenTree::Newline) {
                    iter.next();
                    continue;
                } else {
                    return Err(diagnostic!("expected newline to end statement").into());
                }
            }
        }

        Ok(Self { statements })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ValDeclaration(ValDeclaration),
}

impl Parse<TokenTree> for Statement {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        Ok(Self::ValDeclaration(ValDeclaration::parse(iter)?))
    }
}

parse_impl!(for TokenTree:
    #[derive(Debug, Clone, PartialEq)]
    pub struct ValDeclaration {
        pub val: Val,
        pub ident: Ident,
        pub colon: Colon,
        pub r#type: Ident,
        pub equals: Equals,
        pub init: Expression,
    }
);

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

