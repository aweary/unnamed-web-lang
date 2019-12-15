use std::fmt;

use serde::{Deserialize, Serialize};

use crate::ast::{AssignOp, BinOp, UnOp};
use crate::precedence::Precedence;
use crate::symbol::Symbol;
use codespan::Span;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn follows_item_list(&self) -> bool {
        match self.kind {
            TokenKind::EOF | TokenKind::RCurlyBrace => true,
            _ => false,
        }
    }
    pub fn precedence(&self) -> Precedence {
        use Precedence::*;
        use TokenKind::*;
        match self.kind {
            LParen => ASSIGNMENT,
            Equals => ASSIGNMENT,
            PlusEquals => ASSIGNMENT,
            Dot => ASSIGNMENT,
            QuestionDot => ASSIGNMENT,
            Question => CONDITIONAL,
            Plus => SUM,
            // TODO idk if this is the right precedence
            Or | And | Pipeline => CONDITIONAL,
            Minus => SUM,
            Mul => PRODUCT,
            Div => PRODUCT,
            DblEquals => COMPARE,
            LessThan | GreaterThan => COMPARE,
            _ => NONE,
        }
    }

    /// Translate a token to a binary operator AST node
    pub fn to_bin_op(&self) -> Option<BinOp> {
        use BinOp::*;
        match self.kind {
            TokenKind::Equals => Some(Equals),
            TokenKind::DblEquals => Some(DblEquals),
            TokenKind::Plus => Some(Add),
            TokenKind::Minus => Some(Sub),
            TokenKind::Mul => Some(Mul),
            TokenKind::Div => Some(Div),
            TokenKind::Mod => Some(Mod),
            TokenKind::And => Some(And),
            TokenKind::Or => Some(Or),
            TokenKind::GreaterThan => Some(GreaterThan),
            TokenKind::LessThan => Some(LessThan),
            TokenKind::Pipeline => Some(Pipeline),
            _ => None,
        }
    }

    /// Translate a token to an unary operator AST node
    pub fn to_un_op(&self) -> Option<UnOp> {
        use UnOp::*;
        match self.kind {
            TokenKind::Plus => Some(Plus),
            TokenKind::Minus => Some(Minus),
            TokenKind::Exclaim => Some(Negate),
            _ => None,
        }
    }

    /// Translate a token to an assignment operator AST node
    pub fn to_assign_op(&self) -> Option<AssignOp> {
        use AssignOp::*;
        match self.kind {
            TokenKind::Equals => Some(Equals),
            TokenKind::PlusEquals => Some(PlusEquals),
            _ => None,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[inline]
pub fn token(kind: TokenKind, span: Span) -> Token {
    Token { kind, span }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum TokenKind {
    LexError,
    /* Literals */
    Literal(Lit),
    /* End of file */
    EOF,
    /* Reserved words such as keywords */
    Reserved(Keyword),
    /* Identifier */
    Ident(Symbol),
    /* Free-form text inside a template */
    TemplateText(Symbol),
    // `=>`
    Arrow,
    // `=`
    Equals,
    // `==`
    DblEquals,
    // `+=`,
    PlusEquals,
    // Logical or, `||`
    Or,
    // Logical and, `&&`
    And,
    // Exclaimation mark
    Exclaim,
    // `?.`
    QuestionDot,
    // `|>`
    Pipeline,
    // `|`
    BinOr,
    BinAnd,
    LParen,
    RParen,
    LCurlyBrace,
    RCurlyBrace,
    LBrace,
    RBrace,
    LessThan,
    GreaterThan,
    Colon,
    Semi,
    Dot,
    Mod,
    Caret,
    Question,
    Comma,
    Plus,
    Minus,
    Mul,
    Div,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenKind::*;
        let txt = match *self {
            LCurlyBrace => "{",
            RCurlyBrace => "}",
            LParen => "(",
            RParen => ")",
            Ident(_) => "identifier",
            _ => {
                return write!(f, "{:?}", self);
            }
        };
        write!(f, "{}", txt)
    }
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum Keyword {
    Try,
    Catch,
    Component,
    Else,
    Type,
    For,
    In,
    Enum,
    Func,
    If,
    Import,
    ImportFrom,
    Let,
    Match,
    Return,
    While,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub struct Lit {
    pub kind: LitKind,
    pub symbol: Symbol,
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
pub enum LitKind {
    Bool,
    Number,
    Str,
    // ...
}
