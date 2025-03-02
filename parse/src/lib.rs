use std::iter::Peekable;

use error::Diagnostics;

pub mod lex;
pub mod error;

pub type Result<T> = std::result::Result<T, Diagnostics>;

pub trait Parse<T> {
    fn parse<I>(iter: &mut Peekable<I>) -> Result<Self>
        where I: Iterator<Item = T> + Clone,
              Self: Sized;
}

