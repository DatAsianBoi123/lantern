use std::{fmt::{Display, Formatter}, slice, str::Chars};

use diagnostic::{Diagnostic, FileId, Location, Span, error, symbol::{Symbol, SymbolDisplay, SymbolTable}};

macro_rules! define_keywords {
    ($(#[$meta:meta])* $vis:vis enum $ident:ident { $($keyword:ident = $lit:literal),* $(,)? }) => {
        $(#[$meta])*
        $vis enum $ident {
            $($keyword($keyword)),*
        }

        impl std::fmt::Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $(
                    Self::$keyword(keyword) => write!(f, "`{keyword}`"),
                    )*
                }
            }
        }

        impl $ident {
            pub fn from_str(str: &str, span: diagnostic::Span) -> Option<Self> {
                match str {
                    $(
                    $lit => Some(Self::$keyword($keyword(span))),
                    )*
                    _ => None,
                }
            }

            pub fn span(&self) -> diagnostic::Span {
                match self {
                    $(
                    Self::$keyword($keyword(span)) => *span,
                    )*
                }
            }
        }

        $(
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $keyword(pub ::diagnostic::Span);

        impl TokenKind for $keyword {
            fn name() -> String {
                format!("`{}`", $lit)
            }

            fn from_token(token: Token) -> Option<Self> {
                match token {
                    Token::Keyword($ident::$keyword(keyword)) => Some(keyword),
                    _ => None,
                }
            }

            fn is_token(token: &Token) -> bool {
                matches!(token, Token::Keyword($ident::$keyword(_)))
            }

            fn span(&self) -> Span {
                self.0.clone()
            }
        }

        impl std::fmt::Display for $keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                f.write_str(&Self::name())
            }
        }
        )*
    };
}

macro_rules! define_puncts {
    ($(#[$meta:meta])* $vis:vis enum $ident:ident { $($punct:ident = $lit:literal),* $(,)? }) => {
        $(#[$meta])*
        $vis enum $ident {
            $($punct($punct)),*
        }

        impl std::fmt::Display for $ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $(
                    Self::$punct(punct) => write!(f, "`{punct}`"),
                    )*
                }
            }
        }

        impl $ident {
            pub fn span(&self) -> ::diagnostic::Span {
                match self {
                    $(
                    Self::$punct($punct(span)) => *span,
                    )*
                }
            }
        }

        $(
        #[derive(Debug, Clone, PartialEq, Eq)]
        $vis struct $punct(pub ::diagnostic::Span);

        impl TokenKind for $punct {
            fn name() -> String {
                format!("`{}`", $lit)
            }

            fn from_token(token: Token) -> Option<Self> {
                match token {
                    Token::Punct($ident::$punct(punct)) => Some(punct),
                    _ => None,
                }
            }

            fn is_token(token: &Token) -> bool {
                matches!(token, Token::Punct($ident::$punct(_)))
            }

            fn span(&self) -> Span {
                self.0
            }
        }

        impl std::fmt::Display for $punct {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                f.write_str(&Self::name())
            }
        }
        )*
    };
}

pub trait TokenKind: Sized {
    fn name() -> String;

    fn from_token(token: Token) -> Option<Self>;

    fn is_token(token: &Token) -> bool;

    fn span(&self) -> Span;
}

impl<T: TokenKind> TokenKind for Box<T> {
    fn name() -> String {
        T::name()
    }

    fn from_token(token: Token) -> Option<Self> {
        T::from_token(token).map(Box::new)
    }

    fn is_token(token: &Token) -> bool {
        T::is_token(token)
    }

    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Literal(Literal),
    Keyword(Keyword),
    Ident(Ident),
    Punct(Punct),
    Eof(Eof),
}

impl SymbolDisplay for Token {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        match self {
            Self::Literal(literal) => literal.to_string(),
            Self::Keyword(keyword) => keyword.to_string(),
            Self::Ident(ident) => ident.display(symbol_table),
            Self::Punct(punct) => punct.to_string(),
            Self::Eof(eof) => eof.to_string(),
        }
    }
}

impl TokenKind for Token {
    fn name() -> String {
        "TOKEN".to_string()
    }

    fn from_token(token: Token) -> Option<Self> {
        Some(token)
    }

    fn is_token(_: &Token) -> bool {
        true
    }

    fn span(&self) -> Span {
        match self {
            Self::Literal(literal) => literal.span(),
            Self::Keyword(keyword) => keyword.span(),
            Self::Ident(ident) => ident.span(),
            Self::Punct(punct) => punct.span(),
            Self::Eof(eof) => eof.span(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String, Span),
    Integer(i64, Span),
    Float(f64, Span),
    True(Span),
    False(Span),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(str, _) => write!(f, "{str:?}"),
            Self::Integer(int, _) => write!(f, "{int}"),
            Self::Float(float, _) => write!(f, "{float}"),
            Self::True(_) => write!(f, "`true`"),
            Self::False(_) => write!(f, "`false`"),
        }
    }
}

impl TokenKind for Literal {
    fn name() -> String {
        "LITERAL".to_string()
    }

    fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Literal(lit) => Some(lit),
            _ => None,
        }
    }

    fn is_token(token: &Token) -> bool {
        matches!(token, Token::Literal(_))
    }

    fn span(&self) -> Span {
        match self {
            Self::String(_, span) => *span,
            Self::Integer(_, span) => *span,
            Self::Float(_, span) => *span,
            Self::True(span) => *span,
            Self::False(span) => *span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub Symbol, pub Span);

impl TokenKind for Ident {
    fn name() -> String {
        "IDENT".to_string()
    }

    fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Ident(ident) => Some(ident),
            _ => None,
        }
    }

    fn is_token(token: &Token) -> bool {
        matches!(token, Token::Ident(_))
    }

    fn span(&self) -> Span {
        self.1
    }
}

impl SymbolDisplay for Ident {
    fn display(&self, symbol_table: &SymbolTable) -> String {
        symbol_table.resolve(self.0).to_string()
    }
}

impl Ident {
    pub fn is_valid_char(char: char) -> bool {
        char == '_' || char.is_alphanumeric()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Eof(pub Span);

impl Display for Eof {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<eof>")
    }
}

impl TokenKind for Eof {
    fn name() -> String {
        "<eof>".to_string()
    }

    fn from_token(token: Token) -> Option<Self> {
        match token {
            Token::Eof(eof) => Some(eof),
            _ => None,
        }
    }

    fn is_token(token: &Token) -> bool {
        matches!(token, Token::Eof(_))
    }

    fn span(&self) -> Span {
        self.0
    }
}

define_keywords! {
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Keyword {
        Val = "val",
        If = "if",
        Else = "else",
        While = "while",
        Fun = "fun",
        Using = "using",
        Native = "native",
        Struct = "struct",
        Primitive = "primitive",
        Return = "return",
        Continue = "continue",
        Break = "break",
        Throw = "throw",
    }
}

define_puncts! {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Punct {
        Comma = ',',
        Semi = ';',
        Colon = ':',
        Period = '.',
        Bang = '!',

        Plus = '+',
        Hyphen = '-',
        Asterisk = '*',
        Slash = '/',
        Percent = '%',
        Equals = '=',
        At = '@',

        PlusEq = "+=",
        HyphenEq = "-=",
        AsteriskEq = "*=",
        SlashEq = "/=",
        PercentEq = "%=",

        Less = '<',
        LessEq = "<=",
        Greater = '>',
        GreaterEq = ">=",
        EqualsEquals = "==",
        NotEquals = "!=",

        And = "&&",
        Or = "||",

        ArrowRight = "->",

        OpenParen = '(',
        ClosedParen = ')',
        OpenBracket = '[',
        ClosedBracket = ']',
        OpenBrace = '{',
        ClosedBrace = '}',
    }
}

#[derive(Debug)]
pub struct Lexer<'a, 's> {
    source: FileId,
    symbol_table: &'s mut SymbolTable<'a>,
    chars: Chars<'a>,
    line: u32,
    col: u32,
}

impl<'a, 's> Lexer<'a, 's> {
    pub fn new(source: FileId, input: &'a str, symbol_table: &'s mut SymbolTable<'a>) -> Self {
        Self {
            source,
            symbol_table,
            chars: input.chars(),
            line: 1,
            col: 0,
        }
    }

    pub fn location(&self) -> Location {
        Location {
            line: self.line,
            col: self.col,
        }
    }

    pub fn span_single(&self, loc: Location) -> Span {
        Span::new_single(self.source, loc.line, loc.col)
    }

    pub fn span_from(&self, start: Location) -> Span {
        Span::new(self.source, start, self.location())
    }

    fn peek_char(&mut self) -> Option<char> {
        self.chars.clone().next()
    }

    fn peek2_char(&mut self) -> Option<char> {
        let mut chars = self.chars.clone();
        chars.next();
        chars.next()
    }

    fn next_char(&mut self) -> Option<char> {
        let peek = self.chars.next()?;
        if peek == '\n' {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += 1;
        }
        Some(peek)
    }

    fn peek_is(&mut self, char: char) -> bool {
        self.peek_char().is_some_and(|peek| peek == char)
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_some_and(|char| char.is_whitespace()) {
            self.next_char();
        }
    }

    fn next_escape(&mut self) -> Result<char, Diagnostic> {
        let start = self.location();
        match self.next_char() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('"') => Ok('"'),
            Some(_) => Err(error!(self.span_from(start) => "invalid escape character")),
            None => Err(error!(self.span_from(start) => "expected escape character")),
        }
    }

    pub fn next_int(&mut self, mut num: i64) -> (i64, i32) {
        let mut digits = 1;
        while let Some(next) = self.peek_char() && let Some(digit) = next.to_digit(10) {
            num = num * 10 + digit as i64;
            digits += 1;
            self.next_char();
        }
        (num, digits)
    }

    pub fn next_token(&mut self) -> Result<Token, Diagnostic> {
        macro_rules! punct {
            ($start:expr => $ident:ident) => {
                Token::Punct(Punct::$ident($ident(self.span_from($start))))
            };
        }

        self.skip_whitespace();

        let Some(next) = self.next_char() else {
            // <eof> is always the next character over
            return Ok(Token::Eof(Eof(Span::new_single(self.source, self.line, self.col + 1))));
        };
        let start = self.location();
        match next {
            ',' => Ok(punct!(start => Comma)),
            ';' => Ok(punct!(start => Semi)),
            ':' => Ok(punct!(start => Colon)),
            '.' => Ok(punct!(start => Period)),
            '!' if self.peek_is('=') => {
                let punct = punct!(start => NotEquals);
                self.next_char();
                Ok(punct)
            }
            '!' => Ok(punct!(start => Bang)),

            '+' if self.peek_is('=') => {
                let punct = punct!(start => PlusEq);
                self.next_char();
                Ok(punct)
            }
            '+' => Ok(punct!(start => Plus)),
            '-' if self.peek_is('>') => {
                let punct = punct!(start => ArrowRight);
                self.next_char();
                Ok(punct)
            }
            '-' if self.peek_is('=') => {
                let punct = punct!(start => HyphenEq);
                self.next_char();
                Ok(punct)
            }
            '-' => Ok(punct!(start => Hyphen)),
            '*' if self.peek_is('=') => {
                let punct = punct!(start => AsteriskEq);
                self.next_char();
                Ok(punct)
            }
            '*' => Ok(punct!(start => Asterisk)),
            '/' if self.peek_is('/') => {
                self.next_char();
                while let Some(next) = self.peek_char() && next != '\n' {
                    self.next_char();
                }
                // don't output comment tokens
                self.next_token()
            }
            '/' if self.peek_is('=') => {
                let punct = punct!(start => SlashEq);
                self.next_char();
                Ok(punct)
            }
            '/' => Ok(punct!(start => Slash)),
            '%' if self.peek_is('=') => {
                let punct = punct!(start => PercentEq);
                self.next_char();
                Ok(punct)
            }
            '%' => Ok(punct!(start => Percent)),
            '=' if self.peek_is('=') => {
                let punct = punct!(start => EqualsEquals);
                self.next_char();
                Ok(punct)
            }
            '=' => Ok(punct!(start => Equals)),
            '@' => Ok(punct!(start => At)),

            '<' if self.peek_is('=') => {
                let punct = punct!(start => LessEq);
                self.next_char();
                Ok(punct)
            }
            '<' => Ok(punct!(start => Less)),
            '>' if self.peek_is('=') => {
                let punct = punct!(start => GreaterEq);
                self.next_char();
                Ok(punct)
            }
            '>' => Ok(punct!(start => Greater)),

            '&' if self.peek_is('&') => {
                let punct = punct!(start => And);
                self.next_char();
                Ok(punct)
            }
            '|' if self.peek_is('|') => {
                let punct = punct!(start => Or);
                self.next_char();
                Ok(punct)
            }

            '(' => Ok(punct!(start => OpenParen)),
            ')' => Ok(punct!(start => ClosedParen)),
            '[' => Ok(punct!(start => OpenBracket)),
            ']' => Ok(punct!(start => ClosedBracket)),
            '{' => Ok(punct!(start => OpenBrace)),
            '}' => Ok(punct!(start => ClosedBrace)),

            '"' => {
                match self.next_char() {
                    Some('"') => Ok(Token::Literal(Literal::String(String::new(), self.span_from(start)))),
                    Some(next) => {
                        let mut word = if next == '\\' {
                            self.next_escape()?.to_string()
                        } else {
                            next.to_string()
                        };

                        while let Some(char) = self.peek_char() && char != '\n' {
                            if char == '"' {
                                self.next_char();
                                return Ok(Token::Literal(Literal::String(word, self.span_from(start))));
                            }
                            if char == '\\' {
                                self.next_char();
                                word.push(self.next_escape()?);
                                continue;
                            }
                            word.push(char);
                            self.next_char();
                        }

                        Err(error!(self.span_single(start) => "unclosed quotation marks"))
                    }
                    None => Err(error!(self.span_single(start) => "unclosed quotation marks")),
                }
            }

            next if let Some(num) = next.to_digit(10) => {
                let (num, _) = self.next_int(num as i64);
                if self.peek_is('.') && let Some(decimal) = self.peek2_char().and_then(|char| char.to_digit(10)) {
                    self.next_char(); // .
                    self.next_char(); // [0-9]
                    let (decimal, places) = self.next_int(decimal as i64);
                    Ok(Token::Literal(Literal::Float(num as f64 + decimal as f64 / 10f64.powi(places), self.span_from(start))))
                } else {
                    Ok(Token::Literal(Literal::Integer(num, self.span_from(start))))
                }
            }
            next if Ident::is_valid_char(next) => {
                let str_start = self.chars.as_str().as_ptr();
                let mut len = next.len_utf8();

                while let Some(next) = self.peek_char() && Ident::is_valid_char(next) {
                    len += next.len_utf8();
                    self.next_char();
                }

                // move `start` to include `next`
                // SAFETY: `start` was derived from a &'a str, and it is guaranteed to contain a
                // `len` length character to its left
                let word = unsafe { str::from_utf8_unchecked(slice::from_raw_parts::<'a>(str_start.sub(next.len_utf8()), len)) };

                match word {
                    "true" => Ok(Token::Literal(Literal::True(self.span_from(start)))),
                    "false" => Ok(Token::Literal(Literal::False(self.span_from(start)))),
                    _ if let Some(keyword) = Keyword::from_str(word, self.span_from(start)) => Ok(Token::Keyword(keyword)),
                    _ => Ok(Token::Ident(Ident(self.symbol_table.store(word), self.span_from(start)))),
                }
            }
            next => {
                Err(error!(self.span_single(start) => "invalid character `{next}`"))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{assert_matches, path::PathBuf};

    use diagnostic::SourceMap;

    use crate::*;

    #[test]
    fn test_str() {
        let input = r#""hello there""#;

        let mut source_map = SourceMap::new();
        let root = source_map.add_source(PathBuf::new(), input);
        let mut symbol_table = SymbolTable::new();
        let mut lexer = Lexer::new(root, input, &mut symbol_table);

        assert_matches!(lexer.next_token(), Ok(Token::Literal(Literal::String(str, _))) if str == "hello there");
    }

    #[test]
    fn test_lexer() {
        let input = "val abc: std.int = 10";

        let mut source_map = SourceMap::new();
        let root = source_map.add_source(PathBuf::new(), input);
        let mut symbol_table = SymbolTable::new();
        let mut lexer = Lexer::new(root, input, &mut symbol_table);

        let val = lexer.next_token();
        let ident = lexer.next_token();
        let colon = lexer.next_token();
        assert_matches!(val, Ok(Token::Keyword(Keyword::Val(_))));
        assert_matches!(ident, Ok(Token::Ident(Ident(symbol, _))) if symbol == symbol_table.get("abc").unwrap());
        assert_matches!(colon, Ok(Token::Punct(Punct::Colon(_))));
    }
}

