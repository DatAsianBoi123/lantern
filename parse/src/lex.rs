use std::iter::Peekable;

use crate::{diagnostic, error::{Diagnostic, InvalidTokenError}, Parse, Result};

macro_rules! punct_kind {
    (for $tt_ident: ident : $(#[$meta: meta])* $vis: vis enum $ident: ident { $($punct: ident ( $char: literal )),* $(,)? }) => {
        $(#[$meta])*
        $vis enum $ident {
            $($punct($punct),)*
        }

        impl $ident {
            pub fn is_punct(char: char) -> bool {
                match char {
                    $($char => true,)*
                    _ => false
                }
            }
        }

        impl $crate::Parse<char> for $ident {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self>
            where I: std::iter::Iterator<Item = char>
            {
                match iter.next().expect("next iter") {
                    $($char => { Ok(Self::$punct($punct)) },)*
                    char => Err($crate::diagnostic!("invalid punct `{char}`").into())
                }
            }
        }

        $(
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        $vis struct $punct;

        impl $punct {
            pub fn is(token: &$crate::lex::TokenTree) -> bool {
                match token {
                    $crate::lex::TokenTree::$tt_ident($ident::$punct(_)) => true,
                    _ => false,
                }
            }
        }
        impl $crate::Parse<char> for $punct {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self>
            where I: std::iter::Iterator<Item = char>
            {
                let next = iter.next().expect("next iter");
                if next == $char { Ok(Self) }
                else { Err($crate::diagnostic!("expected punct `{}`", $char).into()) }
            }
        }
        impl $crate::Parse<$crate::lex::TokenTree> for $punct {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self>
            where I: std::iter::Iterator<Item = $crate::lex::TokenTree>
            {
                let next = iter.next().expect("next iter");
                if matches!(next, $crate::lex::TokenTree::$tt_ident($ident::$punct(_))) { Ok(Self) }
                else { Err($crate::diagnostic!("expected punct `{}`", $char).into()) }
            }
        }
        )*
    };
}

macro_rules! impl_parse_token {
    ($ident: ident : $pat: pat = $expr: expr) => {
        impl Parse<$crate::lex::TokenTree> for $ident {
            fn parse<I>(iter: &mut std::iter::Peekable<I>) -> Result<Self>
            where I: std::iter::Iterator<Item = $crate::lex::TokenTree>
            {
                match iter.next().expect("next iter") {
                    $pat => Ok($expr),
                    _ => Err($crate::diagnostic!("unexpected token").into()),
                }
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenTree {
    Group(Group),
    Ident(Ident),
    Literal(Literal),
    Punct(Punct),
    Newline,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub delimiter: Delimiter,
    pub tokens: Vec<TokenTree>,
}

impl_parse_token!(Group: TokenTree::Group(group) = group);

impl Parse<char> for Group {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = char> + Clone
    {
        let delimiter = Delimiter::from_char_open(iter.next().expect("next iter")).ok_or_else(|| diagnostic!("unknown open delimiter"))?;
        let tokens = lex(iter)?;
        let next = iter.next().ok_or_else(|| diagnostic!("missing closing delimiter"))?;
        Delimiter::from_char_closed(next)
            .ok_or_else(|| Diagnostic::new(Box::new(InvalidTokenError(next))))?;
        Ok(Self { delimiter, tokens })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Delimiter {
    /// Parenthesis ()
    Paren,
    /// Brackets &#91;&#93;
    Bracket,
    /// Braces {}
    Brace,
}

impl Delimiter {
    pub fn from_char_open(char: char) -> Option<Self> {
        match char {
            '(' => Some(Self::Paren),
            '[' => Some(Self::Bracket),
            '{' => Some(Self::Brace),
            _ => None,
        }
    }

    pub fn from_char_closed(char: char) -> Option<Self> {
        match char {
            ')' => Some(Self::Paren),
            ']' => Some(Self::Bracket),
            '}' => Some(Self::Brace),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident {
    pub name: String,
}

impl_parse_token!(Ident: TokenTree::Ident(ident) = ident);

impl Parse<char> for Ident {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = char>
    {
        let mut name = String::new();

        while let Some(char) = iter.peek() {
            if !char.is_ascii_alphanumeric() { break; };

            name += &iter.next().expect("next iter").to_string();
        }

        if name.is_empty() { return Err(diagnostic!("expected identifier").into()); };
        Ok(Self { name })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
}

impl_parse_token!(Literal: TokenTree::Literal(literal) = literal);

impl Parse<char> for Literal {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = char> + Clone
    {
        Ok(Self { kind: LiteralKind::parse(iter)? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    String(String),
    Number(f64),
    Boolean(bool),
}

impl Parse<char> for LiteralKind {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
    where I: Iterator<Item = char> + Clone
    {
        let next = iter.next().expect("next iter");

        if next == '"' || next == '\'' {
            let mut string = String::new();
            let mut closing = false;
            let mut escape = false;
            for char in iter {
                if escape {
                    match char {
                        '\\' => string += "\\",
                        'n' => string += "\n",
                        'r' => string += "\r",
                        't' => string += "\t",
                        '"' => string += "\"",
                        '\'' => string += "\'",
                        _ => return Err(diagnostic!("invalid escape character `{char}`").into()),
                    }
                    escape = false;
                    continue;
                }

                if char == '\\' {
                    escape = true;
                    continue;
                }

                if char == next {
                    closing = true;
                    break;
                };
                string += &char.to_string();
            }
            return if closing {
                Ok(Self::String(string))
            } else {
                Err(diagnostic!("missing end quotation").into())
            };
        };

        if let Some((num, _)) = read_int(iter, next) {
            let mut num = num as f64;
            if iter.peek().copied() == Some('.') {
                iter.next();
                let first = iter.next().unwrap();
                let decimal = read_int(iter, first)
                    .map(|(decimal, places)| decimal as f64 / 10_u32.pow(places) as f64)
                    .ok_or_else(|| diagnostic!("expected decimal after ."))?;
                num += decimal;
            }

            return Ok(Self::Number(num));
        };

        if let Ok(Ident { name }) = Ident::parse(iter) {
            match name.as_ref() {
                "true" => return Ok(Self::Boolean(true)),
                "false" => return Ok(Self::Boolean(false)),
                _ => {},
            };
        };

        Err(diagnostic!("unknown literal").into())
    }
}

punct_kind!(for Punct:
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub enum Punct {
        Comma(','),
        Semi(';'),
        Colon(':'),
        Period('.'),
        Question('?'),
        Bang('!'),

        Plus('+'),
        Hyphen('-'),
        Asterisk('*'),
        Slash('/'),
        Percent('%'),
        Equals('='),
    }
);

impl_parse_token!(Punct: TokenTree::Punct(punct) = punct);

pub fn lex<I>(input: &mut Peekable<I>) -> Result<Vec<TokenTree>>
where I: Iterator<Item = char> + Clone
{
    let mut tokens = Vec::new();

    while let Some(&peek) = input.peek() {
        if peek.is_whitespace() {
            if peek == '\n' { tokens.push(TokenTree::Newline); };
            input.next();
            continue;
        }

        if Delimiter::from_char_open(peek).is_some() {
            tokens.push(TokenTree::Group(Group::parse(input)?));
        } else if Punct::is_punct(peek) {
            tokens.push(TokenTree::Punct(Punct::parse(input)?));
        } else if peek == '"' || peek == '\'' || peek.is_ascii_digit() {
            tokens.push(TokenTree::Literal(Literal::parse(input)?));
        } else if peek.is_ascii_alphanumeric() {
            let ident = Ident::parse(input)?;
            if ident.name == "true" {
                tokens.push(TokenTree::Literal(Literal { kind: LiteralKind::Boolean(true) }));
            } else if ident.name == "false" {
                tokens.push(TokenTree::Literal(Literal { kind: LiteralKind::Boolean(false) }));
            } else {
                tokens.push(TokenTree::Ident(ident));
            }
        } else {
            break;
        }
    }

    Ok(tokens)
}

fn read_int<I: Iterator<Item = char>>(iter: &mut Peekable<I>, first: char) -> Option<(u64, u32)> {
    let mut read = 1;
    if let Some(num) = first.to_digit(10) {
        let mut num = num as u64;
        loop {
            let Some(digit) = iter.peek().and_then(|char| char.to_digit(10)) else { break Some((num, read)); };
            num *= 10;
            num += digit as u64;
            iter.next();
            read += 1;
        }
    } else {
        None
    }
}

