use std::iter::Peekable;

use lex::error::Diagnostics;

pub mod lex;

pub trait Parse<T> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
        where I: Iterator<Item = T>,
              Self: Sized;
}

