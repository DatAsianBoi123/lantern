use std::{iter::Peekable, marker::PhantomData};

use expr::Expression;
use macros::Parse;
use token::{Fun, Native, Using, Val};

use crate::{diagnostic, lex::{At, Colon, Comma, Delimiter, Equals, Group, Ident, Period, Punct, TokenTree}, Parse, Result};

pub mod token;
pub mod expr;

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

pub type Block = BraceGroup<Vec<Statement>>;

// TODO: trailing punctuation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctuated<T, P> {
    _phantom: PhantomData<P>,

    pub items: Vec<T>,
}

impl<T, P> Punctuated<T, P> {
    pub fn new(items: Vec<T>) -> Self {
        Self { _phantom: PhantomData, items }
    }
}

impl<T: Parse<TokenTree>, P: Parse<TokenTree>> Parse<TokenTree> for Punctuated<T, P> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let mut items = Vec::new();
        if iter.peek().is_none() { return Ok(Self { _phantom: PhantomData, items }); };
        items.push(T::parse(iter)?);
        while iter.peek().is_some() {
            P::parse(iter)?;
            items.push(T::parse(iter)?);
        };
        Ok(Self { _phantom: PhantomData, items })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub items: Punctuated<Ident, Period>,
}

impl Parse<TokenTree> for Path {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let mut items = Vec::new();
        items.push(Ident::parse(iter)?);
        while let Some(peek) = iter.peek() {
            if !matches!(peek, TokenTree::Punct(Punct::Period(_))) { break; }
            Period::parse(iter)?;
            items.push(Ident::parse(iter)?);
        };
        Ok(Self { items: Punctuated::new(items) })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFile {
    pub statements: Vec<Statement>,
}

impl Parse<TokenTree> for LanternFile {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        Ok(Self { statements: Vec::parse(iter)? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    UsingStatement(UsingStatement),
    ValDeclaration(ValDeclaration),
    FunDefinition(FunDefinition),
    Expression(Expression),
}

impl Parse<TokenTree> for Statement {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = TokenTree> + Clone
    {
        let Some(peek) = iter.peek() else { panic!("unexpected end of iter"); };

        if Val::is(peek) {
            Ok(Self::ValDeclaration(ValDeclaration::parse(iter)?))
        } else if Fun::is(peek) || At::is(peek) {
            Ok(Self::FunDefinition(FunDefinition::parse(iter)?))
        } else if Using::is(peek) {
            Ok(Self::UsingStatement(UsingStatement::parse(iter)?))
        } else {
            Ok(Self::Expression(Expression::parse(iter)?))
        }
    }
}

impl Parse<TokenTree> for Vec<Statement> {
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

        Ok(statements)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Parse)]
pub struct UsingStatement {
    pub using: Using,
    pub module: Path,
    pub colon: Colon,
    pub items: BraceGroup<Punctuated<Ident, Comma>>,
}

#[derive(Debug, Clone, PartialEq, Parse)]
pub struct ValDeclaration {
    pub val: Val,
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Path,
    pub equals: Equals,
    pub init: Expression,
}

#[derive(Debug, Clone, PartialEq, Parse)]
pub struct FunDefinition {
    pub annotations: Vec<ItemAnnotation>,
    pub fun: Fun,
    pub native: Option<Native>,
    pub ident: Ident,
    pub args: ParenGroup<Punctuated<FunArg, Comma>>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq, Parse)]
pub struct FunArg {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Path,
}

#[derive(Debug, Clone, PartialEq, Parse)]
pub struct ItemAnnotation {
    pub at: At,
    pub ident: Ident,
    pub args: ParenGroup<Punctuated<Expression, Comma>>,
}

