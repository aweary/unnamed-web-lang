use crate::pos::Span;
use crate::symbol::Symbol;
use crate::ast::{self, Precedence};
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
    Import,
    ImportFrom,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Token {
        Token { kind, span }
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn follow_stmt_list(&self) -> bool {
        match self.kind {
            TokenKind::EOF | TokenKind::RBrace => true,
            _ => false,
        }
    }

    pub fn precedence(&self) -> Precedence {
        use Precedence::*;
        use TokenKind::*;
        match self.kind {
            LParen => ASSIGNMENT,
            Equals => ASSIGNMENT,
            Dot => ASSIGNMENT,
            Question => CONDITIONAL,
            Plus => SUM,
            // TODO idk if this is the right precedence
            Or | And => CONDITIONAL,
            Minus => SUM,
            Mul => PRODUCT,
            Div => PRODUCT,
            DblEquals => COMPARE,
            LessThan | GreaterThan => COMPARE,
            _ => NONE,
        }
    }

    pub fn to_op(&self) -> Option<ast::Op> {
        use ast::Op::*;
        match self.kind {
            TokenKind::Plus => Some(Add),
            TokenKind::Minus => Some(Sub),
            TokenKind::Mul => Some(Mul),
            TokenKind::Div => Some(Div),
            TokenKind::Mod => Some(Mod),
            TokenKind::And => Some(And),
            TokenKind::Or => Some(Or),
            _ => None
        }
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Token({:#?}) {:#?}", self.kind, self.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // LineComment(&'a str),
    Number(f64),
    Ident(Symbol),
    String(Symbol),
    Keyword(Keyword),
    JSXText(Symbol),
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
    Or,
    And,
}
