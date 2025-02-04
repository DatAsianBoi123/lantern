use std::iter::Peekable;

use error::Diagnostics;

pub mod lex;
pub mod error;

pub trait Parse<T> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self, Diagnostics>
        where I: Iterator<Item = T>,
              Self: Sized;
}

