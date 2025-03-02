use std::{iter::Peekable, marker::PhantomData};

use token::{Val, Fun};

use crate::{diagnostic, lex::{Colon, Comma, Delimiter, Equals, Group, Ident, Literal, TokenTree}, Parse, Result};

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

macro_rules! delimiter {
    ($name: literal : $(#[$meta: meta])* $vis: vis struct $ident: ident ( $delim: ident );) => {
        $(#[$meta])*
        $vis struct $ident<T>(T);

        impl<T: Parse<TokenTree>> Parse<TokenTree> for $ident<T> {
            fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
            where I: Iterator<Item = TokenTree> + Clone
            {
                let group = Group::parse(iter)?;
                if group.delimiter != Delimiter::$delim { return Err(diagnostic!(format!("expected {} group", $name)).into()); };
                let iter = group.tokens.into_iter();
                Ok(Self(T::parse(&mut iter.peekable())?))
            }
        }
    };
}

pub trait DelimiterGroup {
    fn parse_group<I>(iter: &mut Peekable<I>) -> Result<Group>
    where I: Iterator<Item = TokenTree> + Clone;
}

delimiter!("paren":
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct ParenGroup(Paren);
);

delimiter!("bracket":
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct BracketGroup(Bracket);
);

delimiter!("brace":
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct BraceGroup(Brace);
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctuated<T, P> {
    _phantom: PhantomData<P>,

    pub items: Vec<T>,
}

impl<T: Parse<TokenTree>, P: Parse<TokenTree>> Parse<TokenTree> for Punctuated<T, P> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let mut items = Vec::new();
        items.push(T::parse(iter)?);
        while iter.peek().is_some() {
            P::parse(iter)?;
            items.push(T::parse(iter)?);
        };
        Ok(Self { _phantom: PhantomData, items })
    }
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
    FunDefinition(FunDefinition),
}

impl Parse<TokenTree> for Statement {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let Some(peek) = iter.peek() else { panic!("unexpected end of iter"); };

        if Val::is(peek) {
            Ok(Self::ValDeclaration(ValDeclaration::parse(iter)?))
        } else if Fun::is(peek) {
            Ok(Self::FunDefinition(FunDefinition::parse(iter)?))
        } else {
            Err(diagnostic!("unexpected token '{peek:?}'").into())
        }
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

parse_impl!(for TokenTree:
    #[derive(Debug, Clone, PartialEq)]
    pub struct FunDefinition {
        pub fun: Fun,
        pub ident: Ident,
        pub args: ParenGroup<Punctuated<FunArg, Comma>>,
    }
);

parse_impl!(for TokenTree:
    #[derive(Debug, Clone, PartialEq)]
    pub struct FunArg {
        pub ident: Ident,
        pub colon: Colon,
        pub r#type: Ident,
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

