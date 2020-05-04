use std::iter::Peekable;
use std::str::Chars;

use source::diagnostics::{ByteIndex, Span};

#[derive(Debug)]
pub struct Reader<'a> {
    chars: Peekable<Chars<'a>>,
    current_pos: ByteIndex,
}

impl<'a> Reader<'a> {
    pub fn new(source: &'a str) -> Self {
        let chars = source.chars().peekable();
        Reader {
            chars,
            current_pos: ByteIndex(0),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn start(&self) -> ByteIndex {
        self.current_pos
    }

    pub fn end(&self, lo: ByteIndex) -> Span {
        Span::new(lo, self.current_pos)
    }

    pub fn offset(&self) -> ByteIndex {
        self.current_pos
    }
}

impl<'a> Iterator for Reader<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        // TODO fix this so you can just += it
        let pos = self.current_pos.0;
        self.current_pos = ByteIndex(pos + 1);
        self.chars.next()
    }
}
