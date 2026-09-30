use diagnostic::{Diagnostic, FileId, Span, error, symbol::{SymbolDisplay, SymbolTable}};
use lex::{ArrowRight, At, Break, ClosedBrace, ClosedBracket, ClosedParen, Colon, Comma, Continue, Else, Equals, Fun, Ident, If, Keyword, Native, OpenBrace, OpenBracket, OpenParen, Period, Primitive, Punct, Return, Semi, Struct, Throw, Token, TokenKind, Using, Val, While};
use macros::Parse;

use crate::{expr::{Expr, ExprBlock}, stream::{parse_punctuated, TokenStream}};

pub use lex;

pub type Result<T> = std::result::Result<T, Diagnostic>;

pub mod stream;
pub mod expr;

pub trait ParseTokens: Sized {
    fn parse(stream: &mut TokenStream) -> Result<Self>;
}

impl<T: TokenKind> ParseTokens for T {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let next = stream.next_token()?;
        let span = next.span();
        T::from_token(next).ok_or(error!(span => "expected {}", T::name()))
    }
}

pub fn parse<'a>(source: FileId, content: &'a str, symbol_table: &mut SymbolTable<'a>) -> Result<LanternFile> {
    TokenStream::from_input(source, content, symbol_table).parse()
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFile {
    pub stmts: Vec<Stmt>,
}

impl ParseTokens for LanternFile {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut stmts = Vec::new();
        while !stream.is_eof()? {
            stmts.push(stream.parse()?);
        }
        Ok(Self { stmts })
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum Stmt {
    #[parse(using(Fun, Using, Struct, Native, Primitive, At))]
    Item(Item),
    #[parse(using(If))]
    IfStmt(IfStmt),
    #[parse(using(While))]
    WhileStmt(WhileStmt),
    #[parse(using(Val))]
    ValDeclaration(ValDeclaration),
    #[parse(using(Return))]
    Return(ReturnStmt),
    Continue(Continue, Semi),
    Break(Break, Semi),
    Throw(Throw, Expr, Semi),
    Expr(Expr, Semi),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Fun(ItemFun),
    Using(ItemUsing),
    Struct(ItemStruct),
    NativeFun(ItemNativeFun),
    Primitive(ItemPrimitive),
}

impl ParseTokens for Item {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let peek = stream.peek()?;
        match peek {
            Token::Keyword(Keyword::Fun(_)) => stream.parse().map(Self::Fun),
            Token::Keyword(Keyword::Using(_)) => stream.parse().map(Self::Using),
            // TODO: annotations on funs as well
            Token::Keyword(Keyword::Struct(_)) | Token::Punct(Punct::At(_)) => stream.parse().map(Self::Struct),
            Token::Keyword(Keyword::Native(_)) => stream.parse().map(Self::NativeFun),
            Token::Keyword(Keyword::Primitive(_)) => stream.parse().map(Self::Primitive),
            _ => Err(error!(peek.span() => "expected `item`")),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemFun {
    pub fun: Fun,
    pub name: FunName,
    pub open_paren: OpenParen,
    #[parse(with(parse_punctuated::<FunArg, Comma, ClosedParen>))]
    pub args: Vec<FunArg>,
    pub closed_paren: ClosedParen,
    #[parse(with_try(ArrowRight, Type))]
    pub ret: Option<(ArrowRight, Type)>,
    pub block: ExprBlock,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemNativeFun {
    pub native: Native,
    pub fun: Fun,
    pub name: FunName,
    pub open_paren: OpenParen,
    #[parse(with(parse_punctuated::<FunArg, Comma, ClosedParen>))]
    pub args: Vec<FunArg>,
    pub closed_paren: ClosedParen,
    #[parse(with_try(ArrowRight, Type))]
    pub ret: Option<(ArrowRight, Type)>,
    pub semi: Semi,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunName {
    pub base: Option<Type>,
    pub ident: Ident,
}

impl FunName {
    pub fn span(&self) -> Span {
        match &self.base {
            Some(ty) => ty.span().containing(self.ident.span()),
            None => self.ident.span(),
        }
    }
}

impl SymbolDisplay for FunName {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        match &self.base {
            Some(ty) => format!("{}.{}", ty.display(symbol_table), self.ident.display(symbol_table)),
            None => self.ident.display(symbol_table),
        }
    }
}

impl ParseTokens for FunName {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        match stream.peek()? {
            Token::Punct(Punct::OpenParen(_)) => {
                stream.next_token()?;
                let base = stream.parse()?;
                stream.parse::<ClosedParen>()?;
                stream.parse::<Period>()?;
                Ok(Self {
                    base: Some(base),
                    ident: stream.parse()?,
                })
            }
            Token::Punct(Punct::OpenBracket(_)) | Token::Keyword(Keyword::Fun(_)) => {
                let base = stream.parse()?;
                stream.parse::<Period>()?;
                Ok(Self {
                    base: Some(base),
                    ident: stream.parse()?,
                })
            }
            Token::Ident(ident) => {
                let ident = *ident;

                stream.next_token()?;
                if let Ok(Token::Punct(Punct::Period(_))) = stream.peek() {
                    // type.name
                    stream.next_token()?;
                    let mut path: Path = stream.parse()?;
                    path.items.insert(0, ident);

                    let ident = path.items.pop().expect("path has at least 1 element");

                    Ok(Self { base: Some(Type::Path(path)), ident })
                } else {
                    // name
                    Ok(Self { base: None, ident })
                }
            }
            token => Err(error!(token.span() => "expected `FunName`")),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct FunArg {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemPrimitive {
    pub primitive: Primitive,
    pub ident: Ident,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct ItemUsing {
    pub using: Using,
    pub path: Path,
    pub colon: Colon,
    pub open_brace: OpenBrace,
    #[parse(with(parse_punctuated::<Ident, Comma, ClosedBrace>))] // TODO: allow {}?
    pub items: Vec<Ident>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct ItemStruct {
    pub annotations: AnnotationList,
    pub r#struct: Struct,
    pub ident: Ident,
    pub open_brace: OpenBrace,
    #[parse(with(parse_punctuated::<StructField, Comma, ClosedBrace>))]
    pub fields: Vec<StructField>,
    pub closed_brace: ClosedBrace,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ValDeclaration {
    pub val: Val,
    pub ident: Ident,
    #[parse(with_try(Colon, Type))]
    pub r#type: Option<(Colon, Type)>,
    #[parse(with_try(Equals, Expr))]
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
    #[parse(boxed, with_try(Else, IfBranch))]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    pub ret: Return,
    pub expr: Option<Expr>,
    pub semi: Semi,
}

impl ParseTokens for ReturnStmt {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let ret = stream.parse()?;
        let expr = if Semi::is_token(stream.peek()?) {
            None
        } else {
            Some(stream.parse()?)
        };

        Ok(Self {
            ret,
            expr,
            semi: stream.parse()?,
        })
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum IfBranch {
    #[parse(using(If))]
    ElseIf(#[parse(boxed(IfStmt))] Box<IfStmt>),
    #[parse(using(OpenBrace))]
    Else(ExprBlock),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnotationList {
    pub annotations: Vec<Annotation>,
}

impl ParseTokens for AnnotationList {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut annotations = Vec::new();

        while At::is_token(stream.peek()?) {
            annotations.push(stream.parse()?);
        }

        Ok(Self { annotations })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Annotation {
    pub at: At,
    pub ident: Ident,
    pub args: Option<AnnotationArgs>,
}

impl ParseTokens for Annotation {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let at = stream.parse()?;
        let ident = stream.parse()?;
        let args = if OpenParen::is_token(stream.peek()?) {
            Some(stream.parse()?)
        } else {
            None
        };
        Ok(Self {
            at,
            ident,
            args,
        })
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct AnnotationArgs {
    pub open_paren: OpenParen,
    #[parse(with(parse_punctuated::<Ident, Comma, ClosedParen>))]
    pub args: Vec<Ident>,
    pub closed_paren: ClosedParen,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Array(OpenBracket, #[parse(boxed(Type))] Box<Type>, ClosedBracket),
    #[parse(using(Fun))]
    Fun(FunType),
    #[parse(using(Ident))]
    Path(Path),
}

impl Type {
    pub fn span(&self) -> Span {
        match self {
            Self::Array(open_bracket, _, closed_bracket) => open_bracket.span().containing(closed_bracket.span()),
            Self::Fun(fun_type) => fun_type.span(),
            Self::Path(path) => path.span(),
        }
    }
}

impl SymbolDisplay for Type {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        match self {
            Self::Array(_, inner, _) => format!("[{}]", inner.display(symbol_table)),
            Self::Fun(FunType { args, ret, .. }) => {
                let mut string = format!("fun({})", args.iter().map(|arg| arg.display(symbol_table)).collect::<Vec<_>>().join(", "));
                if let Some((_, ret)) = ret {
                    string += &format!(" -> {}", ret.display(symbol_table));
                }
                string
            },
            Self::Path(path) => path.display(symbol_table),
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub struct FunType {
    pub fun: Fun,
    pub open_paren: OpenParen,
    #[parse(with(parse_punctuated::<Type, Comma, ClosedParen>))]
    pub args: Vec<Type>,
    pub closed_paren: ClosedParen,
    #[parse(boxed, with_try(ArrowRight, Type))]
    pub ret: Option<(ArrowRight, Box<Type>)>,
}

impl FunType {
    pub fn span(&self) -> Span {
        let end = self.ret.as_ref().map_or(self.closed_paren.span(), |(_, ty)| ty.span());
        self.fun.span().containing(end)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    pub items: Vec<Ident>,
}

impl SymbolDisplay for Path {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        self.items.iter()
            .map(|item| symbol_table.resolve(item.0))
            .collect::<Vec<_>>()
            .join(".")
    }
}

impl ParseTokens for Path {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut items = vec![stream.parse()?];
        while Period::is_token(stream.peek()?) {
            stream.parse::<Period>()?;
            items.push(stream.parse()?);
        }
        Ok(Self { items })
    }
}

impl Path {
    pub fn first(&self) -> Ident {
        *self.items.first().unwrap()
    }

    pub fn last(&self) -> Ident {
        *self.items.last().unwrap()
    }

    pub fn span(&self) -> Span {
        if self.items.len() == 1 {
            self.first().span()
        } else {
            self.first().span().containing(self.last().span())
        }
    }
}

