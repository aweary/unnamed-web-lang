use std::iter::Peekable;
use std::str::Chars;

use crate::pos::{Pos, Span};

#[derive(Debug)]
pub struct Reader<'a> {
    chars: Peekable<Chars<'a>>,
    current_pos: Pos,
}

impl<'a> Reader<'a> {
    pub fn new(chars: Chars<'a>) -> Self {
        let current_pos = Pos::start();
        let chars = chars.peekable();
        Reader { current_pos, chars }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn start(&self) -> Pos {
        self.current_pos.clone()
    }

    pub fn end(&self, start: Pos) -> Span {
        let end = self.current_pos.clone();
        Span::new(start, end)
    }
}

impl<'a> Iterator for Reader<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let ch = self.chars.next();
        if let Some(ch) = ch {
            if ch == '\n' {
                self.current_pos.line += 1;
                self.current_pos.column = 0;
            } else {
                self.current_pos.column += 1;
            }
            self.current_pos.offset += 1;
        }
        ch
    }
}
