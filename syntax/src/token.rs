use std::fmt;

use serde::{Deserialize, Serialize};

use crate::ast::{AssignOp, BinOp, UnOp};
use crate::precedence::Precedence;
use crate::symbol::Symbol;
use source::diagnostics::Span;

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
        use Precedence::{ASSIGNMENT, COMPARE, CONDITIONAL, NONE, PREFIX, PRODUCT, SUM};
        use TokenKind::{And, BinOr, DblEquals, Div, Dot, Equals, GreaterThan, LCurlyBrace, LParen, LessThan, Minus, Mul, Or, Pipeline, Plus, PlusEquals, Question, QuestionDot};
        match self.kind {
            LCurlyBrace | LParen => PREFIX,
            Equals => ASSIGNMENT,
            PlusEquals => ASSIGNMENT,
            Dot => ASSIGNMENT,
            QuestionDot => ASSIGNMENT,
            Question => CONDITIONAL,
            Plus => SUM,
            // TODO idk if this is the right precedence
            Or | And | Pipeline | BinOr => CONDITIONAL,
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
        use BinOp::{Add, And, BinOr, DblEquals, Div, Equals, GreaterThan, LessThan, Mod, Mul, Or, Pipeline, Sub};
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
            TokenKind::BinOr => Some(BinOr),
            _ => None,
        }
    }

    /// Translate a token to an unary operator AST node
    pub fn to_un_op(&self) -> Option<UnOp> {
        use UnOp::{Minus, Negate, Plus};
        match self.kind {
            TokenKind::Plus => Some(Plus),
            TokenKind::Minus => Some(Minus),
            TokenKind::Exclaim => Some(Negate),
            _ => None,
        }
    }

    /// Translate a token to an assignment operator AST node
    pub fn to_assign_op(&self) -> Option<AssignOp> {
        use AssignOp::{Equals, PlusEquals};
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
        use TokenKind::{Ident, LCurlyBrace, LParen, RCurlyBrace, RParen};
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
    As,
    ImportFrom,
    Let,
    State,
    Match,
    Return,
    While,
    Pub,
    Const,
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
