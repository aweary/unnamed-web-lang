use std::iter::Iterator;

use colored::*;
use ucd::Codepoint;

use crate::reader::Reader;
use crate::token::{Keyword, Token, TokenKind};

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a String,
    chars: Reader<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String) -> Self {
        let chars = source.chars();
        Lexer {
            source,
            chars: Reader::new(chars),
        }
    }

    fn skip_while<F>(&mut self, pred: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.chars.peek() {
                // matches predicate, skip it
                Some(&ch) if pred(ch) => {
                    self.chars.next();
                }
                _ => return,
            }
        }
    }

    fn read_into_while<F>(&mut self, out: &mut String, pred: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.chars.peek() {
                Some(&ch) if pred(ch) => {
                    // eat;
                    // TODO error handling
                    let ch = self.chars.next().unwrap();
                    out.push(ch);
                }
                _ => return,
            }
        }
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(|ch| Codepoint::is_whitespace(ch));
    }

    fn read_numeric_literal(&mut self) -> Option<Token> {
        let span_start = self.chars.start();
        let mut num_str = String::new();
        // TODO floating point numbers
        self.read_into_while(&mut num_str, |ch| ch.is_digit(10));
        // TODO error handling
        let num = num_str.parse::<u32>().unwrap();
        let kind = TokenKind::NumericLiteral(num);
        let span = self.chars.end(span_start);
        let token = Token::new(kind, span);
        Some(token)
    }

    fn read_punc(&mut self, kind: TokenKind) -> Option<Token> {
        // eat
        let span_start = self.chars.start();
        self.chars.next();
        let span = self.chars.end(span_start);
        let token = Token::new(kind, span);
        Some(token)
    }

    fn read_ident(&mut self) -> Option<Token> {
        let span_start = self.chars.start();
        let mut ident = String::new();
        self.read_into_while(&mut ident, |ch| {
            Codepoint::is_id_start(ch) || Codepoint::is_id_continue(ch)
        });
        let span = self.chars.end(span_start);
        let kind = match ident.as_ref() {
            "let" => TokenKind::Keyword(Keyword::Let),
            _ => TokenKind::Ident(ident),
        };
        let token = Token::new(kind, span);
        Some(token)
    }

    pub fn next(&mut self) -> Option<Token> {
        use TokenKind::*;
        self.skip_whitespace();
        match self.chars.peek() {
            // base-10 numeric literals
            Some(&ch) if ch.is_digit(10) => self.read_numeric_literal(),
            // Identifier
            Some(&ch) if Codepoint::is_id_start(ch) => self.read_ident(),
            // Punctuation
            Some('+') => self.read_punc(Plus),
            Some('-') => self.read_punc(Minus),
            Some('*') => self.read_punc(Star),
            Some('/') => self.read_punc(Slash),
            Some('=') => self.read_punc(Equals),
            Some('(') => self.read_punc(LParen),
            Some(')') => self.read_punc(RParen),
            Some('{') => self.read_punc(LBrace),
            Some('}') => self.read_punc(RBrace),
            Some('<') => self.read_punc(LAngle),
            Some('>') => self.read_punc(RAngle),
            Some(':') => self.read_punc(Colon),
            Some(';') => self.read_punc(Semi),
            Some('.') => self.read_punc(Dot),
            Some('%') => self.read_punc(Mod),
            Some('^') => self.read_punc(Caret),
            Some('?') => self.read_punc(Question),
            // EOF
            None => None,
            // Unknown character
            Some(_) => self.unknown_char_panic(),
        }
    }

    // This should be moved out into error.rs or something
    fn unknown_char_panic(&mut self) -> ! {
        let pos = self.chars.start();
        let ch = self.chars.next().unwrap();
        let line = pos.line;
        let lines: Vec<&str> = self.source.split('\n').collect();
        let error_line = lines.get(line - 1).unwrap();
        let point_error_line = format!(
            "{:width$}{pointer}",
            "",
            width = pos.column,
            pointer = "^".red()
        );
        let error_prefix = "Unknown character:".red().bold();
        let line_str = "line".blue();
        let column_str = "column".green();
        panic!(
            "\n\n{} '{}' at {} {}, {} {}\n\n{}\n{}\n\n",
            error_prefix,
            ch,
            line_str,
            pos.line,
            column_str,
            pos.column,
            error_line,
            point_error_line,
        );
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        self.next()
    }
}
