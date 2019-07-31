use crate::pos::Span;
use std::fmt;

#[derive(PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token({:?}) - ({},{})-({},{})",
            self.kind,
            self.span.start.line,
            self.span.start.column,
            self.span.end.line,
            self.span.end.column
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    NumericLiteral(u32),
    Ident(String),
    Keyword(Keyword),
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    Mod,
    LAngle,
    RAngle,
    Semi,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Dot,
    Colon,
    Caret,
    Question,
}
