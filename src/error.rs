use crate::token::Token;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedToken(Token),
    UnexpectedEOF,
}
