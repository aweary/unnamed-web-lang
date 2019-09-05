use crate::pos::Span;
use std::fmt;

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Let,
    Func,
    Return,
    If,
    Else,
    Match,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }

    pub fn follow_stmt_list(&self) -> bool {
        match self.kind {
            TokenKind::EOF | TokenKind::RBrace => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token({:?})", self.kind,)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    NumericLiteral(u32),
    Ident(String),
    StringLiteral(String),
    Keyword(Keyword),
    JSXText(String),
    Arrow,
    Comma,
    Equals,
    DblEquals,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Semi,
    LBrace,
    RBrace,
    LParen,
    RParen,
    Dot,
    Colon,
    Caret,
    Question,
    EOF,
}
