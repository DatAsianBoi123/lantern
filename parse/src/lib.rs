use std::fmt::{Display, Formatter};

use error::Diagnostics;
use macros::Parse;

use crate::{error::Span, expr::{Expr, ExprBlock}, keyword::{Break, Else, False, Fun, If, Native, Return, Struct, True, Using, Val, While}, punct::{ArrowRight, ClosedBrace, ClosedParen, Colon, Comma, DoubleSlash, Equals, OpenBrace, OpenParen, Period, Semi}, stream::{AlphabeticWord, AlphanumericWord, Char, Digit, Not, Not2, Punctuated, Repetition, StreamBranch, TokenStream, TrailingDenied, Whitespace}};

pub mod stream;
pub mod punct;
pub mod keyword;
pub mod expr;
pub mod error;

pub type Result<T> = std::result::Result<T, Diagnostics>;

pub trait ParseTokens {
    fn parse(stream: &mut StreamBranch) -> Result<Self>
    where Self: Sized;
}

pub fn parse(content: &str) -> Result<LanternFile> {
    parse_stream(TokenStream::from_input(content))
}

pub fn parse_stream(mut input: TokenStream) -> Result<LanternFile> {
    let mut branch = input.branch();

    let file = LanternFile::parse(&mut branch)?;
    branch.commit();

    match input.peek() {
        Some(char) => Err(diagnostic!(input.span(), "unexpected token `{char}`").into()),
        None => Ok(file),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LanternFile {
    pub stmts: Vec<Stmt>,
}

impl ParseTokens for LanternFile {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        let mut stmts = Vec::new();
        while !stream.is_empty() {
            let _ = <Repetition<0, Whitespace>>::parse(stream);
            stmts.push(Stmt::parse(stream)?);
            let _ = <Repetition<0, Whitespace>>::parse(stream);
        }
        Ok(Self { stmts })
    }
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum Item {
    Fun(ItemFun),
    Using(ItemUsing),
    Struct(ItemStruct),
    Native(ItemNative),
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemFun {
    pub fun: Fun,
    pub ident: Ident,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, FunArg, Comma>,
    pub closed_paren: ClosedParen,
    pub ret: Option<(ArrowRight, Type)>,
    pub block: ExprBlock,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ItemNative {
    pub native: Native,
    pub fun: Fun,
    pub ident: Ident,
    pub open_paren: OpenParen,
    pub args: Punctuated<0, FunArg, Comma>,
    pub closed_paren: ClosedParen,
    pub ret: Option<(ArrowRight, Type)>,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct FunArg {
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
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
    Comment(DoubleSlash, Vec<Not<'\n'>>),
    Reassign(Reassign),
    Return(Return, Expr, Semi),
    Break(Break, Semi),
    Expr(Expr, Semi),
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct ValDeclaration {
    pub val: Val,
    pub ident: Ident,
    pub colon: Colon,
    pub r#type: Type,
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

#[derive(Parse, Debug, Clone, PartialEq)]
pub struct Reassign {
    pub ident: Ident,
    pub eq: Equals,
    pub expr: Expr,
    pub semi: Semi,
}

#[derive(Parse, Debug, Clone, PartialEq)]
pub enum Literal {
    String(QuotedString),
    Number(Number),
    Boolean(Boolean),
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
#[from(QuotedStringRaw)]
pub struct QuotedString(pub String, pub Span);

impl From<QuotedStringRaw> for QuotedString {
    fn from(value: QuotedStringRaw) -> Self {
        let string = value.chars.0.into_iter()
            .fold(String::new(), |mut acc, curr| {
                match curr {
                    QuotedStringCharRaw::EscapeChar(_, EscapeChar::Newline(_)) => acc.push('\n'),
                    QuotedStringCharRaw::EscapeChar(_, EscapeChar::CarriageReturn(_)) => acc.push('\r'),
                    QuotedStringCharRaw::EscapeChar(_, EscapeChar::Tab(_)) => acc.push('\t'),
                    QuotedStringCharRaw::EscapeChar(_, EscapeChar::Backslash(_)) => acc.push('\\'),
                    QuotedStringCharRaw::EscapeChar(_, EscapeChar::Quote(_)) => acc.push('\"'),
                    QuotedStringCharRaw::Char(Not2(char, _)) => acc.push(char),
                };
                acc
            });

        Self(string, value.open_quote.1)
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
struct QuotedStringRaw {
    open_quote: Char<'"'>,
    chars: Repetition<0, QuotedStringCharRaw>,
    end_quote: Char<'"'>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
enum QuotedStringCharRaw {
    EscapeChar(Char<'\\'>, EscapeChar),
    Char(Not2<'\\', '"'>),
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
enum EscapeChar {
    Newline(Char<'n'>),
    CarriageReturn(Char<'r'>),
    Tab(Char<'t'>),
    Backslash(Char<'\\'>),
    Quote(Char<'"'>),
}

#[derive(Parse, Debug, Clone, PartialEq)]
#[from(NumberRaw)]
pub struct Number(pub f64, pub Span);

impl From<NumberRaw> for Number {
    fn from(value: NumberRaw) -> Self {
        let whole = value.whole.0.into_iter()
            .fold(0, |acc, digit| acc * 10 + digit.0);
        let decimal = value.decimal
            .map(|decimal| decimal.1.0.into_iter()
                .fold((0, 0), |(acc, i), digit| (acc * 10 + digit.0, i + 1)))
            .map(|(decimal, places)| decimal as f64 / 10f64.powi(places))
            .unwrap_or(0.0);

        Self(whole as f64 + decimal, value.whole.1)
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
struct NumberRaw {
    whole: Repetition<1, Digit>,
    decimal: Option<(Char<'.'>, Repetition<1, Digit>)>,
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum Boolean {
    True(True),
    False(False),
}

impl Boolean {
    pub fn span(&self) -> &Span {
        match self {
            Self::True(True(span)) | Self::False(False(span)) => span,
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Fun(FunType),
    Path(Path),
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String, pub Span);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.0)
    }
}

impl ParseTokens for Ident {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        let IdentWord(string, span) = IdentWord::parse(stream)?;
        if !keyword::is_keyword(&string) {
            Ok(Self(string, span))
        } else {
            Err(diagnostic!(span, "unexpected keyword `{string}`").into())
        }
    }
}

#[derive(Parse, Debug, Clone, PartialEq, Eq)]
#[from(WordIdentRaw)]
pub struct IdentWord(pub String, pub Span);

impl From<WordIdentRaw> for IdentWord {
    fn from(value: WordIdentRaw) -> Self {
        Self(value.first.0.to_string() + value.rest.into_iter().map(|char| char.0).collect::<String>().as_ref(), value.first.1)
    }
}

#[derive(Parse, Debug, PartialEq, Eq)]
struct WordIdentRaw {
    first: AlphabeticWord,
    rest: Vec<AlphanumericWord>,
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use crate::{keyword::{Fun, Native, Using}, stream::TokenStream};

    use super::*;

    #[test]
    fn test_number() {
        assert_ok_empty("123", Number(123.0, Span::new(1, 1)));
        assert_ok_empty("0", Number(0.0, Span::new(1, 1)));
        assert_ok_empty("2", Number(2.0, Span::new(1, 1)));
        assert_ok_empty("32000", Number(32000.0, Span::new(1, 1)));
        assert_ok_empty("00076", Number(76.0, Span::new(1, 1)));

        assert_ok_empty("3.22", Number(3.22, Span::new(1, 1)));
        assert_ok_empty("0.51", Number(0.51, Span::new(1, 1)));
        assert_ok_empty("2.0001", Number(2.0001, Span::new(1, 1)));
        assert_ok_empty("999.99", Number(999.99, Span::new(1, 1)));
        assert_ok_empty("5.800", Number(5.8, Span::new(1, 1)));

        assert_ok_partial("3.", Number(3.0, Span::new(1, 1)));
        assert_ok_partial("9..", Number(9.0, Span::new(1, 1)));
        assert_ok_partial("9.200.", Number(9.2, Span::new(1, 1)));
        assert_ok_partial("58.abc", Number(58.0, Span::new(1, 1)));
        assert_ok_partial("5,300.22", Number(5.0, Span::new(1, 1)));
        assert_ok_partial("20 .11", Number(20.0, Span::new(1, 1)));

        assert_err::<Number>("a100");
        assert_err::<Number>("");
        assert_err::<Number>(".");
        assert_err::<Number>("-50.2");
        assert_err::<Number>("iiiii");
    }

    #[test]
    fn test_keyword() {
        assert_ok_empty("fun", Fun(Span::new(1, 1)));
        assert_ok_empty("val  \n", Val(Span::new(1, 1)));
        assert_ok_empty("    true", True(Span::new(5, 1)));
        assert_ok_empty("\n   fun   ", Fun(Span::new(4, 2)));

        assert_ok_partial("using   a", Using(Span::new(1, 1)));
        assert_ok_partial("false f", False(Span::new(1, 1)));
        assert_ok_partial("native   fun", Native(Span::new(1, 1)));
        assert_ok_partial("fun   .", Fun(Span::new(1, 1)));

        assert_err::<Native>("nativee");
        assert_err::<True>("trueeee");
        assert_err::<Using>("   funafun");
        assert_err::<False>("fail  ");
    }

    #[test]
    fn test_quoted_string() {
        assert_ok_empty(r#""hello""#, QuotedString("hello".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""""#, QuotedString(String::new(), Span::new(1, 1)));
        assert_ok_empty(r#""  hi  ""#, QuotedString("  hi  ".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""hmmm.. ""#, QuotedString("hmmm.. ".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""hello with \"quotes\"""#, QuotedString("hello with \"quotes\"".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""new\nline""#, QuotedString("new\nline".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""return! \r carriage!""#, QuotedString("return! \r carriage!".to_owned(), Span::new(1, 1)));
        assert_ok_empty(r#""tab\ttab\ttab""#, QuotedString("tab\ttab\ttab".to_owned(), Span::new(1, 1)));

        assert_ok_partial(r#""hello" world"#, QuotedString("hello".to_owned(), Span::new(1, 1)));
        assert_ok_partial(r#""" world world"#, QuotedString(String::new(), Span::new(1, 1)));

        assert_err::<QuotedString>(r#""no end quote!"#);
        assert_err::<QuotedString>(r#"no, "start quote?""#);
        assert_err::<QuotedString>(r#""invalid \a escape char""#);
    }

    fn assert_ok_empty<T: ParseTokens + Debug + PartialEq>(input: &str, eq: T) {
        match parse_with(input) {
            (Ok(t), stream) => {
                assert!(stream.is_empty());
                assert_eq!(eq, t);
            },
            (Err(err), _) => panic!("parse error: {err}"),
        }
    }

    fn assert_ok_partial<T: ParseTokens + Debug + PartialEq>(input: &str, eq: T) {
        match parse_with(input) {
            (Ok(t), stream) => {
                assert!(!stream.is_empty());
                assert_eq!(eq, t);
            },
            (Err(err), _) => panic!("parse error: {err}"),
        }
    }

    fn assert_err<T: ParseTokens>(input: &str) {
        let (res, stream) = parse_with::<T>(input);
        assert!(res.is_err());
        assert_eq!(input.len(), stream.len());
    }

    fn parse_with<T: ParseTokens>(input: &str) -> (Result<T>, TokenStream) {
        let mut stream = TokenStream::from_input(input);
        let mut branch = stream.branch();
        match T::parse(&mut branch) {
            Ok(t) => {
                branch.commit();
                (Ok(t), stream)
            },
            Err(err) => (Err(err), stream),
        }
    }
}

