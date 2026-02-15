use std::{marker::PhantomData};

use lex::{Lexer, Token};

use crate::{ParseTokens, Result};

mod private {
    pub trait Sealed {}
}

#[derive(Debug, Clone)]
pub struct TokenStream<'a> {
    lexer: Lexer<'a>,
    peek: Option<Result<Token>>,
}

impl<'a> TokenStream<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self { lexer, peek: None }
    }

    pub fn from_input(str: &'a str) -> Self {
        Self::new(Lexer::new(str))
    }

    pub fn is_eof(&mut self) -> Result<bool> {
        Ok(matches!(self.peek()?, Token::Eof(_)))
    }

    pub fn peek(&mut self) -> Result<&Token> {
        self.peek.get_or_insert_with(|| self.lexer.next_token()).as_ref().map_err(Clone::clone)
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.peek.take().unwrap_or_else(|| self.lexer.next_token())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Repetition<const L: usize, T>(pub Vec<T>);

impl<const L: usize, T: ParseTokens> ParseTokens for Repetition<L, T> {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut items = Vec::new();
        for _ in 0..L {
            items.push(T::parse(stream)?);
        }
        while T::can_parse(stream.peek()?) {
            items.push(T::parse(stream)?);
        }
        Ok(Self(items))
    }

    fn can_parse(peek: &Token) -> bool {
        if L == 0 {
            true
        } else {
            T::can_parse(peek)
        }
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
pub struct Punctuated<const L: usize, T, P, B: TrailingBehavior = TrailingAllowed>(pub Vec<T>, PhantomData<B>, PhantomData<P>);

impl<const L: usize, T: ParseTokens, P: ParseTokens> ParseTokens for Punctuated<L, T, P, TrailingAllowed> {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut items = Vec::new();

        if L > 0 {
            items.push(T::parse(stream)?);
            for _ in 0..L - 1 {
                P::parse(stream)?;
                items.push(T::parse(stream)?);
            }
        } else if T::can_parse(stream.peek()?) {
            items.push(T::parse(stream)?);
        } else {
            return Ok(Self(items, PhantomData, PhantomData));
        }

        while P::can_parse(stream.peek()?) {
            P::parse(stream)?;
            if T::can_parse(stream.peek()?) {
                items.push(T::parse(stream)?);
            } else {
                break;
            }
        }
        Ok(Self(items, PhantomData, PhantomData))
    }

    fn can_parse(peek: &Token) -> bool {
        if L == 0 {
            true
        } else {
            T::can_parse(peek)
        }
    }
}

impl<const L: usize, T: ParseTokens, P: ParseTokens> ParseTokens for Punctuated<L, T, P, TrailingDenied> {
    fn parse(stream: &mut TokenStream) -> Result<Self> {
        let mut items = Vec::new();

        if L > 0 {
            items.push(T::parse(stream)?);
            for _ in 0..L - 1 {
                P::parse(stream)?;
                items.push(T::parse(stream)?);
            }
        } else if T::can_parse(stream.peek()?) {
            items.push(T::parse(stream)?);
        } else {
            return Ok(Self(items, PhantomData, PhantomData));
        }

        while P::can_parse(stream.peek()?) {
            P::parse(stream)?;
            items.push(T::parse(stream)?);
        }
        Ok(Self(items, PhantomData, PhantomData))
    }

    fn can_parse(peek: &Token) -> bool {
        if L == 0 {
            true
        } else {
            T::can_parse(peek)
        }
    }
}

#[cfg(test)]
mod tests {
    use lex::Literal;

    use crate::*;

    #[test]
    fn test_punctuated() {
        let input = "1,23,4";
        let mut stream = TokenStream::from_input(input);

        let parsed = <Punctuated<0, Literal, Comma, TrailingDenied>>::parse(&mut stream);
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap().0.len(), 3);
    }

    #[test]
    fn test_stream() {
        let input = "val a: std.int = 10;";
        let mut stream = TokenStream::from_input(input);

        let val = ValDeclaration::parse(&mut stream);
        dbg!(&val);
        assert!(matches!(val, Ok(ValDeclaration { .. })));
    }
}

