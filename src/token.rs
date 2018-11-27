#[derive(Debug, PartialEq)]
pub enum ReservedWord {
    Component,
    State,
    Effect
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Assign,
    Colon,
    Comma,
    Dot,
    EOF,
    Exclaim,
    ForwardSlash,
    Ident(String),
    Reserved(ReservedWord),
    LCaret,
    LCurlyBracket,
    LParen,
    RCaret,
    RCurlyBracket,
    RParen,
    SemiColon,
    String(String),
    Number(i64),
}
