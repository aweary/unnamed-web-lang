use std::error::Error;
use std::fmt;

use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum LexError {
    UnexpectedCharacter(char),
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "LexError")
    }
}

impl Error for LexError {
    fn description(&self) -> &str {
        "LexError"
    }

    fn cause(&self) -> Option<&Error> {
        None
    }
}

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
