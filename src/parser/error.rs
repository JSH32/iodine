use std::result;

use thiserror::Error;

use crate::lexer;

pub type Result<T> = result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Lexer error: {0}")]
    LexerError(lexer::error::Error),
    #[error("Parser found unexpected token: {got}, expected: {want}")]
    UnexpectedToken { 
        want: String, 
        got: String
    }
}
