#[derive(Debug, Clone, PartialEq)]
pub struct Pos {
    pub offset: usize,
    pub column: usize,
    pub line: usize,
}

impl Pos {
    pub fn start() -> Self {
        Pos {
            offset: 0,
            column: 0,
            line: 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}

impl Span {
    pub fn new(start: Pos, end: Pos) -> Self {
        Span { start, end }
    }
}
