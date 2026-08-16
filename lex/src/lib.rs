use std::{fmt::{Display, Formatter}, str::Chars};

use diagnostic::{Diagnostic, Span, error};

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
                    Self::$keyword($keyword(span)) => span.clone(),
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
                    Self::$punct($punct(span)) => span.clone(),
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
                self.0.clone()
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

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Literal(literal) => write!(f, "{literal}"),
            Self::Keyword(keyword) => write!(f, "{keyword}"),
            Self::Ident(ident) => write!(f, "{ident}"),
            Self::Punct(punct) => write!(f, "{punct}"),
            Self::Eof(eof) => write!(f, "{eof}"),
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
            Self::Ident(Ident(_, span)) => span.clone(),
            Self::Punct(punct) => punct.span(),
            Self::Eof(Eof(span)) => span.clone(),
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
            Self::String(_, span) => span.clone(),
            Self::Integer(_, span) => span.clone(),
            Self::Float(_, span) => span.clone(),
            Self::True(span) => span.clone(),
            Self::False(span) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String, pub Span);

impl Display for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}`", self.0)
    }
}

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
        self.1.clone()
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
        self.0.clone()
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

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    line: u32,
    col: u32,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars(),
            line: 1,
            col: 0,
        }
    }

    pub fn span(&self) -> Span {
        Span::new(self.line, self.col)
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
        match self.next_char() {
            Some('n') => Ok('\n'),
            Some('r') => Ok('\r'),
            Some('t') => Ok('\t'),
            Some('\\') => Ok('\\'),
            Some('"') => Ok('"'),
            Some(_) => Err(error!(self.span() => "invalid escape character")),
            None => Err(error!(self.span() => "expected escape character")),
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
            ($ident:ident) => {
                Token::Punct(Punct::$ident($ident(self.span())))
            };
        }

        self.skip_whitespace();

        let Some(next) = self.next_char() else {
            // <eof> is always the next character over
            return Ok(Token::Eof(Eof(Span::new(self.line, self.col + 1))));
        };
        match next {
            ',' => Ok(punct!(Comma)),
            ';' => Ok(punct!(Semi)),
            ':' => Ok(punct!(Colon)),
            '.' => Ok(punct!(Period)),
            '!' if self.peek_is('=') => {
                self.next_char();
                Ok(punct!(NotEquals))
            },
            '!' => Ok(punct!(Bang)),

            '+' => Ok(punct!(Plus)),
            '-' if self.peek_is('>') => {
                let punct = punct!(ArrowRight);
                self.next_char();
                Ok(punct)
            }
            '-' => Ok(punct!(Hyphen)),
            '*' => Ok(punct!(Asterisk)),
            '/' if self.peek_is('/') => {
                // comment, ignore characters until newline
                self.next_char();
                let mut comment = String::new();
                while let Some(next) = self.peek_char() && next != '\n' {
                    comment.push(next);
                    self.next_char();
                }
                // don't output comment tokens
                self.next_token()
            }
            '/' => Ok(punct!(Slash)),
            '%' => Ok(punct!(Percent)),
            '=' if self.peek_is('=') => {
                let punct = punct!(EqualsEquals);
                self.next_char();
                Ok(punct)
            }
            '=' => Ok(punct!(Equals)),

            '<' if self.peek_is('=') => {
                let punct = punct!(LessEq);
                self.next_char();
                Ok(punct)
            }
            '<' => Ok(punct!(Less)),
            '>' if self.peek_is('=') => {
                let punct = punct!(GreaterEq);
                self.next_char();
                Ok(punct)
            }
            '>' => Ok(punct!(Greater)),

            '&' if self.peek_is('&') => {
                let punct = punct!(And);
                self.next_char();
                Ok(punct)
            }
            '|' if self.peek_is('|') => {
                let punct = punct!(Or);
                self.next_char();
                Ok(punct)
            }

            '(' => Ok(punct!(OpenParen)),
            ')' => Ok(punct!(ClosedParen)),
            '[' => Ok(punct!(OpenBracket)),
            ']' => Ok(punct!(ClosedBracket)),
            '{' => Ok(punct!(OpenBrace)),
            '}' => Ok(punct!(ClosedBrace)),

            '"' => {
                let span = self.span();
                match self.next_char() {
                    Some('"') => Ok(Token::Literal(Literal::String(String::new(), span))),
                    Some(next) => {
                        let mut word = if next == '\\' {
                            self.next_escape()?.to_string()
                        } else {
                            next.to_string()
                        };

                        while let Some(char) = self.peek_char() && char != '\n' {
                            if char == '"' {
                                self.next_char();
                                return Ok(Token::Literal(Literal::String(word, span)))
                            }
                            if char == '\\' {
                                self.next_char();
                                word.push(self.next_escape()?);
                                continue;
                            }
                            word.push(char);
                            self.next_char();
                        }

                        Err(error!(span => "unclosed quotation marks"))
                    }
                    None => Err(error!(span => "unclosed quotation marks")),
                }
            }

            next => {
                let span = self.span();
                if let Some(num) = next.to_digit(10) {
                    let (num, _) = self.next_int(num as i64);
                    if self.peek_is('.') && self.peek2_char().is_some_and(|char| char.is_ascii_digit()) {
                        self.next_char();
                        let decimal = self.next_char().expect("decimal exists").to_digit(10).expect("is ascii digit");
                        let (decimal, places) = self.next_int(decimal as i64);
                        Ok(Token::Literal(Literal::Float(num as f64 + decimal as f64 / 10f64.powi(places), span)))
                    } else {
                        Ok(Token::Literal(Literal::Integer(num, span)))
                    }
                } else if Ident::is_valid_char(next) {
                    let mut word = next.to_string();

                    while let Some(next) = self.peek_char() && Ident::is_valid_char(next) {
                        word.push(next);
                        self.next_char();
                    }

                    match word.as_ref() {
                        "true" => Ok(Token::Literal(Literal::True(span))),
                        "false" => Ok(Token::Literal(Literal::True(span))),
                        _ => {
                            if let Some(keyword) = Keyword::from_str(&word, span.clone()) {
                                Ok(Token::Keyword(keyword))
                            } else {
                                Ok(Token::Ident(Ident(word, span)))
                            }
                        }
                    }
                } else {
                    Err(error!(span => "invalid character `{next}`"))
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_str() {
        let mut lexer = Lexer::new(r#""hello there""#);

        assert_eq!(lexer.next_token(), Ok(Token::Literal(Literal::String("hello there".to_string(), Span::new(1, 1)))));
    }

    #[test]
    fn test_lexer() {
        let mut lexer = Lexer::new("val abc: std.int = 10");

        assert_eq!(lexer.next_token(), Ok(Token::Keyword(Keyword::Val(Val(Span::new(1, 1))))));
        assert_eq!(lexer.next_token(), Ok(Token::Ident(Ident("abc".to_string(), Span::new(1, 5)))));
        assert_eq!(lexer.next_token(), Ok(Token::Punct(Punct::Colon(Colon(Span::new(1, 8))))));
    }
}

