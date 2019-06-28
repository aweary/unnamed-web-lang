#[derive(Debug, PartialEq)]
pub enum ReservedWord {
    Component,
    Type,
    State,
    Effect,
    Const,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    TernaryCondition,
    Add,
    Assign,
    Colon,
    Comma,
    Dot,
    EOF,
    Exclaim,
    ForwardSlash,
    GreaterThan,
    Ident(String),
    LCurlyBracket,
    LessThan,
    LParen,
    Number(i64),
    RCurlyBracket,
    Reserved(ReservedWord),
    RParen,
    SemiColon,
    String(String),
}
