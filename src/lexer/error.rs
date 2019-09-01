use std::error::Error;
use std::fmt;

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
