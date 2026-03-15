use std::fmt::{Display, Formatter};

use diagnostic::{Diagnostic, error};
use lex::{ArrowRight, Break, ClosedBrace, ClosedBracket, ClosedParen, Colon, Comma, Continue, Else, Equals, Fun, Ident, If, Keyword, Native, OpenBrace, OpenBracket, OpenParen, Period, Punct, Return, Semi, Struct, Token, TokenKind, Using, Val, While};
use macros::Parse;

use crate::{expr::{Expr, ExprBlock}, stream::{Punctuated, TokenStream, TrailingDenied}};

pub use lex;

pub type Result<T> = std::result::Result<T, Diagnostic>;

pub mod stream;
pub mod expr;

pub trait ParseTokens: Sized {
    fn parse(stream: &mut TokenStream) -> Result<Self>;

    fn can_parse(peek: &Token) -> bool;
}

// TODO: find impl for Box<T>, Option<T>, and (A, B, ...)
impl<T: TokenKind> ParseTokens for T {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let next = stream.next_token()?;
        let span = next.span();
        T::from_token(next).ok_or(error!(span => "expected {}", T::name()))
    }

    fn can_parse(peek: &Token) -> bool {
        T::is_token(peek)
    }
}

pub fn parse(content: &str) -> Result<LanternFile> {
    LanternFile::parse(&mut TokenStream::from_input(content))
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFile {
    pub stmts: Vec<Stmt>,
}

impl ParseTokens for LanternFile {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut stmts = Vec::new();
        while !stream.is_eof()? {
            stmts.push(Stmt::parse(stream)?);
        }
        Ok(Self { stmts })
    }

    fn can_parse(token: &Token) -> bool {
        !matches!(token, Token::Eof(_))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fun(ItemFun),
    Using(ItemUsing),
    Struct(ItemStruct),
    NativeFun(ItemNativeFun),
    NativeStruct(ItemNativeStruct),
}

impl ParseTokens for Item {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let peek = stream.peek()?;
        match peek {
            Token::Keyword(Keyword::Fun(_)) => ItemFun::parse(stream).map(Self::Fun),
            Token::Keyword(Keyword::Using(_)) => ItemUsing::parse(stream).map(Self::Using),
            Token::Keyword(Keyword::Struct(_)) => ItemStruct::parse(stream).map(Self::Struct),
            Token::Keyword(Keyword::Native(_)) => {
                let Token::Keyword(Keyword::Native(native)) = stream.next_token()? else { unreachable!() };
                match stream.next_token()? {
                    Token::Keyword(Keyword::Fun(fun)) => {
                        Ok(Self::NativeFun(ItemNativeFun {
                            native,
                            fun,
                            ident: ParseTokens::parse(stream)?,
                            open_paren: ParseTokens::parse(stream)?,
                            args: ParseTokens::parse(stream)?,
                            closed_paren: ParseTokens::parse(stream)?,
                            ret: if matches!(stream.peek()?, Token::Punct(Punct::ArrowRight(_))) {
                                Some((ArrowRight::parse(stream)?, Type::parse(stream)?))
                            } else {
                                None
                            },
                            semi: ParseTokens::parse(stream)?,
                        }))
                    },
                    Token::Keyword(Keyword::Struct(r#struct)) => {
                        Ok(Self::NativeStruct(ItemNativeStruct {
                            native,
                            r#struct,
                            ident: ParseTokens::parse(stream)?,
                            semi: ParseTokens::parse(stream)?,
                        }))
                    },
                    token => Err(error!(token.span() => "expected `fun` or `struct`")),
                }
            }
            _ => Err(error!(peek.span() => "expected `item`")),
        }
    }

    fn can_parse(peek: &Token) -> bool {
        matches!(peek, Token::Keyword(Keyword::Fun(_) | Keyword::Using(_) | Keyword::Struct(_) | Keyword::Native(_)))
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemFun {
    pub fun: Fun,
    pub path: Path,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, FunArg, Comma>,
    pub closed_paren: ClosedParen,
    #[parse({
        if ArrowRight::can_parse(stream.peek()?) {
            Some((ArrowRight::parse(stream)?, Type::parse(stream)?))
        } else {
            None
        }
    })]
    pub ret: Option<(ArrowRight, Type)>,
    pub block: ExprBlock,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemNativeFun {
    pub native: Native,
    pub fun: Fun,
    pub ident: Ident,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, FunArg, Comma>,
    pub closed_paren: ClosedParen,
    #[parse({
        if ArrowRight::can_parse(stream.peek()?) {
            Some((ArrowRight::parse(stream)?, Type::parse(stream)?))
        } else {
            None
        }
    })]
    pub ret: Option<(ArrowRight, Type)>,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct FunArg {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemNativeStruct {
    pub native: Native,
    pub r#struct: Struct,
    pub ident: Ident,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct ItemUsing {
    pub using: Using,
    pub path: Path,
    pub colon: Colon,
    pub open_brace: OpenBrace,
    pub items: Punctuated<1, Ident, Comma>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct ItemStruct {
    pub r#struct: Struct,
    pub ident: Ident,
    pub open_brace: OpenBrace,
    pub fields: Punctuated<0, StructField, Comma>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum Stmt {
    Item(Item),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    ValDeclaration(ValDeclaration),
    Return(Return, Expr, Semi),
    Continue(Continue, Semi),
    Break(Break, Semi),
    Expr(Expr, Semi),
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ValDeclaration {
    pub val: Val,
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
    #[parse({
        if Equals::can_parse(stream.peek()?) {
            Some((Equals::parse(stream)?, Expr::parse(stream)?))
        } else {
            None
        }
    })]
    pub init: Option<(Equals, Expr)>,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub r#if: If,
    pub open_paren: OpenParen,
    pub condition: Expr,
    pub closed_paren: ClosedParen,
    pub block: ExprBlock,
    #[parse({
        if Else::can_parse(stream.peek()?) {
            Some((Else::parse(stream)?, Box::new(IfBranch::parse(stream)?)))
        } else {
            None
        }
    })]
    pub branch: Option<(Else, Box<IfBranch>)>,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub r#while: While,
    pub open_paren: OpenParen,
    pub condition: Expr,
    pub closed_paren: ClosedParen,
    pub block: ExprBlock,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum IfBranch {
    ElseIf(IfStmt),
    Else(ExprBlock),
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Array(OpenBracket, #[parse(Box::new(Type::parse(stream)?))] Box<Type>, ClosedBracket),
    Fun(FunType),
    Path(Path),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Array(_, inner, _) => write!(f, "[{inner}]"),
            Self::Fun(FunType { args, ret, .. }) => {
                write!(f, "fun({})", args.0.iter().map(|arg| arg.to_string()).collect::<Vec<_>>().join(", "))?;
                if let Some((_, ret)) = ret {
                    write!(f, " -> {ret}")?;
                }
                Ok(())
            },
            Self::Path(path) => path.fmt(f),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    pub fun: Fun,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, Type, Comma>,
    pub closed_paren: ClosedParen,
    #[parse({
        if ArrowRight::can_parse(stream.peek()?) {
            Some((ArrowRight::parse(stream)?, Box::new(Type::parse(stream)?)))
        } else {
            None
        }
    })]
    pub ret: Option<(ArrowRight, Box<Type>)>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub items: Punctuated<1, Ident, Period, TrailingDenied>,
}

impl Display for Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.items.0.iter()
            .take(self.items.0.len() - 1)
            .try_for_each(|item| write!(f, "{}.", item.0))?;
        write!(f, "{}", self.items.0.last().expect("path has at least 1 item").0)?;
        Ok(())
    }
}

impl Path {
    pub fn last(&self) -> &Ident {
        self.items.0.last().unwrap()
    }

    pub fn into_last(mut self) -> Ident {
        self.items.0.pop().unwrap()
    }
}

