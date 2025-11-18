use std::{fs, marker::PhantomData, path::Path};

use crate::{ParseTokens, Result, diagnostic, error::{EndOfInputError, Span}};

mod private {
    pub trait Sealed {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TokenStream {
    items: Vec<char>,
    line: usize,
    col: usize,
}

impl TokenStream {
    pub fn new(mut items: Vec<char>) -> Self {
        // front of vec is now the back,
        // faster popping since we only ever pop from the front
        items.reverse();
        Self {
            items,
            line: 1,
            col: 1,
        }
    }

    pub fn from_input(str: &str) -> Self {
        Self::new(str.chars().collect())
    }

    pub fn from_file(path: impl AsRef<Path>) -> std::io::Result<Self> {
        let contents = fs::read_to_string(path)?;
        Ok(Self::from_input(&contents))
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn span(&self) -> Span {
        Span::new(self.col, self.line)
    }

    pub fn peek(&self) -> Option<char> {
        self.items.last().copied()
    }

    pub fn branch(&mut self) -> StreamBranch<'_> {
        StreamBranch::new(self)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StreamBranch<'a> {
    stream: &'a mut TokenStream,
    cursor: usize,

    line: usize,
    col: usize,
}

impl<'a> StreamBranch<'a> {
    fn new(stream: &'a mut TokenStream) -> Self {
        Self {
            line: stream.line,
            col: stream.col,

            stream,
            cursor: 0,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.remaining() == 0
    }

    pub fn remaining(&self) -> usize {
        self.stream.len() - self.cursor
    }

    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn span(&self) -> Span {
        Span::new(self.col, self.line)
    }

    pub fn location(&self) -> Location {
        Location { cursor: self.cursor, col: self.col, line: self.line }
    }

    pub fn goto(&mut self, Location { cursor, col, line }: Location) {
        self.cursor = cursor;
        self.col = col;
        self.line = line;
    }

    pub fn read_one(&mut self) -> Result<char> {
        if self.cursor == self.stream.len() { return Err(diagnostic!(self.span() => EndOfInputError).into()); };

        let next = *self.stream.items.get(self.stream.len() - self.cursor - 1).expect("cursor is within bounds");
        if next == '\n' {
            self.line += 1;
            self.col = 0;
        }
        self.col += 1;
        self.cursor += 1;
        Ok(next)
    }

    pub fn try_read_one<T, F>(&mut self, filter_map: F) -> Result<T>
    where F: Fn(char, Span) -> Result<T>
    {
        if self.cursor == self.stream.len() { return Err(diagnostic!(self.span() => EndOfInputError).into()); };

        let next = *self.stream.items.get(self.stream.len() - self.cursor - 1).expect("cursor is within bounds");
        let result = filter_map(next, self.span());
        if result.is_ok() {
            if next == '\n' {
                self.line += 1;
                self.col = 0;
            }
            self.col += 1;
            self.cursor += 1;
        }
        result
    }

    pub fn commit(self) {
        self.stream.line = self.line;
        self.stream.col = self.col;
        self.stream.items.truncate(self.stream.len() - self.cursor);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Location {
    cursor: usize,
    col: usize,
    line: usize,
}

impl Location {
    pub fn cursor(&self) -> usize {
        self.cursor
    }

    pub fn col(&self) -> usize {
        self.col
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Whitespace(pub Span);

impl ParseTokens for Whitespace {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| char.is_whitespace()
            .then_some(Self(span.clone()))
            .ok_or_else(|| diagnostic!(span, "expected whitespace").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphanumericWord(pub char, pub Span);

impl ParseTokens for AlphanumericWord {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| (char == '_' || char.is_ascii_alphanumeric())
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "invalid ascii alphanumeric char `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AlphabeticWord(pub char, pub Span);

impl ParseTokens for AlphabeticWord {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| (char == '_' || char.is_ascii_alphabetic())
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "invalid ascii alphanumeric char `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alphanumeric(pub char, pub Span);

impl ParseTokens for Alphanumeric {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| char.is_ascii_alphanumeric()
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "invalid ascii alphanumeric char `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Alphabetic(pub char, pub Span);

impl ParseTokens for Alphabetic {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| char.is_ascii_alphabetic()
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "invalid ascii alphabetic char `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Digit(pub u32, pub Span);

impl ParseTokens for Digit {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| char.to_digit(10)
            .map(|digit| Self(digit, span.clone()))
            .ok_or_else(|| diagnostic!(span, "invalid digit `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Char<const C: char>(pub char, pub Span);

impl<const C: char> ParseTokens for Char<C> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| (char == C)
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "expected token `{}`", C).into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Not<const C: char>(pub char, pub Span);

impl<const C: char> ParseTokens for Not<C> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| (char != C)
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "unexpected token `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Not2<const A: char, const B: char>(pub char, pub Span);

impl<const A: char, const B: char> ParseTokens for Not2<A, B> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.try_read_one(|char, span| (char != A && char != B)
            .then_some(Self(char, span.clone()))
            .ok_or_else(|| diagnostic!(span, "unexpected token `{char}`").into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Repetition<const L: usize, T>(pub Vec<T>, pub Span);

impl<const L: usize, T: ParseTokens> ParseTokens for Repetition<L, T> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        // TODO: start + end loc for spans
        let span = stream.span();
        let mut items = Vec::new();
        for _ in 0..L {
            items.push(T::parse(stream)?);
        }
        let mut mark = stream.location();
        while let Ok(item) = T::parse(stream) {
            mark = stream.location();
            items.push(item);
        }
        stream.goto(mark);
        Ok(Self(items, span))
    }
}

pub trait TrailingBehavior: private::Sealed {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TrailingAllowed;
impl private::Sealed for TrailingAllowed {}
impl TrailingBehavior for TrailingAllowed {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TrailingDenied;
impl private::Sealed for TrailingDenied {}
impl TrailingBehavior for TrailingDenied {}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Punctuated<const L: usize, T, P, B: TrailingBehavior = TrailingAllowed>(pub Vec<T>, pub Span, PhantomData<B>, PhantomData<P>);

impl<const L: usize, T: ParseTokens, P: ParseTokens> ParseTokens for Punctuated<L, T, P, TrailingAllowed> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        // TODO: ended span
        let span = stream.span();
        let mut items = Vec::new();

        let mut mark = stream.location();
        if L > 0 {
            items.push(T::parse(stream)?);
            for _ in 0..L - 1 {
                let (_, item) = <(P, T)>::parse(stream)?;
                items.push(item);
            }
        } else if let Ok(item) = T::parse(stream) {
            items.push(item);
        } else {
            stream.goto(mark);
            return Ok(Self(items, span, PhantomData, PhantomData));
        }

        mark = stream.location();
        if P::parse(stream).is_ok() {
            while let Ok(item) = T::parse(stream) {
                items.push(item);
                mark = stream.location();
                if P::parse(stream).is_err() {
                    break;
                }
                mark = stream.location();
            }
        }
        stream.goto(mark);
        Ok(Self(items, span, PhantomData, PhantomData))
    }
}

impl<const L: usize, T: ParseTokens, P: ParseTokens> ParseTokens for Punctuated<L, T, P, TrailingDenied> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        // TODO: ended span
        let span = stream.span();
        let mut items = Vec::new();

        let mut mark = stream.location();
        if L > 0 {
            items.push(T::parse(stream)?);
            for _ in 0..L - 1 {
                let (_, item) = <(P, T)>::parse(stream)?;
                items.push(item);
            }
        } else if let Ok(item) = T::parse(stream) {
            items.push(item);
        } else {
            stream.goto(mark);
            return Ok(Self(items, span, PhantomData, PhantomData));
        }

        mark = stream.location();
        while let Ok((_, item)) = <(P, T)>::parse(stream) {
            mark = stream.location();
            items.push(item);
        }
        stream.goto(mark);
        Ok(Self(items, span, PhantomData, PhantomData))
    }
}

impl ParseTokens for char {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        stream.read_one()
    }
}

impl<T: ParseTokens> ParseTokens for Box<T> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        T::parse(stream).map(Box::new)
    }
}

impl<T: ParseTokens> ParseTokens for Option<T> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        let mark = stream.cursor;
        match T::parse(stream) {
            Ok(item) => Ok(Some(item)),
            Err(_) => {
                stream.cursor = mark;
                Ok(None)
            },
        }
    }
}

impl<T: ParseTokens> ParseTokens for Vec<T> {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        <Repetition<0, T>>::parse(stream).map(|repetition| repetition.0)
    }
}

impl<A: ParseTokens, B: ParseTokens> ParseTokens for (A, B) {
    fn parse(stream: &mut StreamBranch) -> Result<Self> {
        Ok((A::parse(stream)?, B::parse(stream)?))
    }
}

