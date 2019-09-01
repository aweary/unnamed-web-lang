use crate::token::Token;

use std::error::Error;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    UnexpectedEOF,
    // TODO ref to LexError
    LexError,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ParseError")
    }
}

impl Error for ParseError {
    fn description(&self) -> &str {
        match *self {
            ParseError::UnexpectedEOF => "Unexpected EOF",
            // TODO how to read token out of this?
            ParseError::UnexpectedToken(_) => "Unexpected token",
            ParseError::LexError => "Lex Error",
        }
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}
