use lex::{Lexer, Token, TokenKind};

use crate::{ParseTokens, Result};

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

pub fn parse_repetition<T: ParseTokens, E: TokenKind>(stream: &mut TokenStream) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while !E::is_token(stream.peek()?) {
        items.push(T::parse(stream)?);
    }
    Ok(items)
}

pub fn parse_punctuated<T: ParseTokens, P: ParseTokens, E: TokenKind>(stream: &mut TokenStream) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while !E::is_token(stream.peek()?) {
        items.push(T::parse(stream)?);
        if E::is_token(stream.peek()?) {
            break;
        }
        P::parse(stream)?;
    }
    Ok(items)
}

pub fn parse_punctuated_untrailed<T: ParseTokens, P: ParseTokens, E: TokenKind>(stream: &mut TokenStream) -> Result<Vec<T>> {
    if E::is_token(stream.peek()?) {
        return Ok(Vec::new());
    }
    let mut items = vec![T::parse(stream)?];
    while !E::is_token(stream.peek()?) {
        P::parse(stream)?;
        items.push(T::parse(stream)?);
    }
    Ok(items)
}

#[cfg(test)]
mod tests {
    use lex::{Eof, Literal};

    use crate::{stream::parse_punctuated_untrailed, *};

    #[test]
    fn test_punctuated() {
        let input = "1,23,4";
        let mut stream = TokenStream::from_input(input);

        let parsed = parse_punctuated_untrailed::<Literal, Comma, Eof>(&mut stream);
        assert!(parsed.is_ok());
        assert_eq!(parsed.unwrap().len(), 3);
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

