use std::{num, result};

use thiserror::Error;

/// Specialized result type for lexer
pub type Result<T> = result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Illegal floating point number: {0}")]
    IllegalFloat(num::ParseFloatError),
    #[error("Illegal number radix: {0}")]
    IllegalIntegerRadix(char),
    #[error("Illegal integer number: {0}")]
    IllegalInteger(num::ParseIntError),
}
