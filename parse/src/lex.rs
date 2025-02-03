use std::iter::Peekable;

use error::{Diagnostic, Diagnostics, InvalidTokenError};

use crate::{diagnostic, Parse};

pub mod error;

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

impl Parse<char> for Group {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
    where I: Iterator<Item = char>
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
    Paren,
    Bracket,
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

impl Parse<char> for Ident {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
    where I: Iterator<Item = char>
    {
        let mut name = String::new();

        while let Some(char) = iter.peek() {
            if !char.is_ascii_alphanumeric() { break; };

            name += &iter.next().expect("next iter").to_string();
        }

        if name.is_empty() { panic!("empty ident"); };
        Ok(Self { name })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    pub kind: LiteralKind,
}

impl Parse<char> for Literal {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
    where I: Iterator<Item = char>
    {
        Ok(Self { kind: LiteralKind::parse(iter)? })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    String(String),
    Number(f64),
}

impl Parse<char> for LiteralKind {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
    where I: Iterator<Item = char>,
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

        Err(diagnostic!("unknown literal").into())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punct {
    pub kind: PunctKind,
}

impl Parse<char> for Punct {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
    where I: Iterator<Item = char>
    {
        let next = iter.next().expect("next iter");
        Ok(Self { kind: PunctKind::from_char(next).ok_or_else(|| diagnostic!("unknown punctuation {next}"))? })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PunctKind {
    Comma,
    Semi,
    Colon,
    Period,
    Question,
    Bang,

    Plus,
    Hyphen,
    Asterisk,
    Slash,
    Equals,
}

impl PunctKind {
    fn from_char(char: char) -> Option<Self> {
        match char {
            ',' => Some(Self::Comma),
            ';' => Some(Self::Semi),
            ':' => Some(Self::Colon),
            '.' => Some(Self::Period),
            '?' => Some(Self::Question),
            '!' => Some(Self::Bang),

            '+' => Some(Self::Plus),
            '-' => Some(Self::Hyphen),
            '*' => Some(Self::Asterisk),
            '/' => Some(Self::Slash),
            '=' => Some(Self::Equals),

            _ => None,
        }
    }
}

pub fn lex<I>(input: &mut Peekable<I>) -> Result<Vec<TokenTree>, Diagnostics>
where I: Iterator<Item = char>
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
        } else if peek == '"' || peek == '\'' || peek.is_ascii_digit() {
            tokens.push(TokenTree::Literal(Literal::parse(input)?));
        } else if PunctKind::from_char(peek).is_some() {
            tokens.push(TokenTree::Punct(Punct::parse(input)?));
        } else if peek.is_ascii_alphanumeric() {
            tokens.push(TokenTree::Ident(Ident::parse(input)?));
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

