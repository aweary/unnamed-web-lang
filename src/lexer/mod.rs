use std::collections::VecDeque;
use std::iter::Iterator;

pub mod error;

use colored::*;
use ucd::Codepoint;

use crate::reader::Reader;
use crate::token::{Keyword, Token, TokenKind};

use error::LexError;

type Result<T> = std::result::Result<T, LexError>;

// We need to be able to track when we're inside a JSX tag
// TODO make this a bitflag
#[derive(Debug)]
struct JSXState {
    seen_less_than: bool,
    seen_ident: bool,
    seen_greater_than: bool,
    in_jsx_tag: bool,
}

impl JSXState {
    #[inline]
    pub fn new() -> Self {
        JSXState {
            seen_less_than: false,
            seen_ident: false,
            seen_greater_than: false,
            in_jsx_tag: false,
        }
    }
    pub fn reset(&mut self) {
        self.seen_less_than = false;
        self.seen_ident = false;
        self.seen_greater_than = false;
        self.in_jsx_tag = false;
    }
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a String,
    chars: Reader<'a>,
    lookahead: VecDeque<Token>,
    jsx_state: JSXState,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String) -> Self {
        let chars = source.chars();
        Lexer {
            source,
            chars: Reader::new(chars),
            lookahead: VecDeque::with_capacity(4),
            jsx_state: JSXState::new(),
        }
    }

    pub fn peek(&mut self) -> Result<&Token> {
        // TODO is this the best way to model a lookahead?
        if self.lookahead.is_empty() {
            let token = self.next()?;
            self.lookahead.push_front(token);
        }
        Ok(self.lookahead.front().unwrap())
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

    fn read_numeric_literal(&mut self) -> Result<Token> {
        let span_start = self.chars.start();
        let mut num_str = String::new();
        // TODO floating point numbers
        self.read_into_while(&mut num_str, |ch| ch.is_digit(10));
        // TODO error handling
        let num = num_str.parse::<u32>().unwrap();
        let kind = TokenKind::NumericLiteral(num);
        let span = self.chars.end(span_start);
        let token = Token::new(kind, span);
        Ok(token)
    }

    fn read_punc(&mut self, kind: TokenKind) -> Result<Token> {
        let span_start = self.chars.start();
        // eat
        self.chars.next();
        // Manage JSX state
        match kind {
            TokenKind::LessThan => {
                self.jsx_state.seen_less_than = true;
            }
            TokenKind::Div => {
                self.jsx_state.reset();
                // Reset if we're in a closing tag
            }
            TokenKind::GreaterThan => {
                self.jsx_state.in_jsx_tag =
                    self.jsx_state.seen_less_than && self.jsx_state.seen_ident;
            }
            _ => (),
        };
        let span = self.chars.end(span_start);
        let token = Token::new(kind, span);
        Ok(token)
    }

    fn read_equals(&mut self) -> Result<Token> {
        println!("read equals");
        let span_start = self.chars.start();
        // Eat..
        let eat = self.chars.next();
        println!("eating {:?}", eat);
        match self.chars.peek() {
            Some('=') => {
                self.chars.next();
                let span = self.chars.end(span_start);
                let token = Token::new(TokenKind::DblEquals, span);
                Ok(token)
            }
            _ => {
                let span = self.chars.end(span_start);
                let token = Token::new(TokenKind::Equals, span);
                Ok(token)
            }
        }
    }

    fn read_ident(&mut self) -> Result<Token> {
        let span_start = self.chars.start();
        let mut ident = String::new();
        self.read_into_while(&mut ident, |ch| {
            Codepoint::is_id_start(ch) || Codepoint::is_id_continue(ch)
        });
        let span = self.chars.end(span_start);
        let kind = match ident.as_ref() {
            "let" => TokenKind::Keyword(Keyword::Let),
            "fn" | "component" => TokenKind::Keyword(Keyword::Func),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            _ => TokenKind::Ident(ident),
        };
        let token = Token::new(kind, span);
        self.jsx_state.seen_ident = true;
        Ok(token)
    }

    pub fn next(&mut self) -> Result<Token> {
        match self.lookahead.pop_front() {
            Some(token) => Ok(token),
            None => self.next_token(),
        }
    }

    fn read_string(&mut self) -> Result<Token> {
        let start = self.chars.start();
        // todo expect to be "
        self.chars.next();
        let mut string = String::new();
        self.read_into_while(&mut string, |ch| ch != '"');
        // todo expect to be "
        self.chars.next();
        let span = self.chars.end(start);
        Ok(Token::new(TokenKind::StringLiteral(string), span))
    }

    fn read_jsx_text(&mut self) -> Result<Token> {
        let start = self.chars.start();
        let mut text = String::new();
        self.read_into_while(&mut text, |ch| match ch {
            '}' | '{' | '<' | '>' => false,
            _ => true,
        });
        let span = self.chars.end(start);
        self.jsx_state.reset();
        // Dont output JSXText token if theres no text
        if text.is_empty() {
            self.next_token()
        } else {
            Ok(Token::new(TokenKind::JSXText(text), span))
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        self.skip_whitespace();
        let in_jsx_tag = self.jsx_state.in_jsx_tag;
        let token = match self.chars.peek() {
            Some(_) if in_jsx_tag => {
                let token = self.read_jsx_text()?;
                self.jsx_state.reset();
                Ok(token)
            }
            // base-10 numeric literals
            Some(&ch) if ch.is_digit(10) => self.read_numeric_literal(),
            // Identifier
            Some(&ch) if Codepoint::is_id_start(ch) => self.read_ident(),
            // String
            Some('"') => self.read_string(),
            // Punctuation
            Some('+') => self.read_punc(Plus),
            Some('-') => self.read_punc(Minus),
            Some('*') => self.read_punc(Mul),
            Some('/') => self.read_punc(Div),
            Some('=') => self.read_equals(),
            Some('(') => self.read_punc(LParen),
            Some(')') => self.read_punc(RParen),
            Some('{') => self.read_punc(LBrace),
            Some('}') => self.read_punc(RBrace),
            Some('<') => self.read_punc(LessThan),
            Some('>') => self.read_punc(GreaterThan),
            Some(':') => self.read_punc(Colon),
            Some(';') => self.read_punc(Semi),
            Some('.') => self.read_punc(Dot),
            Some('%') => self.read_punc(Mod),
            Some('^') => self.read_punc(Caret),
            Some('?') => self.read_punc(Question),
            Some(',') => self.read_punc(Comma),
            // EOF
            None => {
                // todo bad way to handle span
                let start = self.chars.start();
                let span = self.chars.end(start);
                Ok(Token::new(TokenKind::EOF, span))
            }
            // Unknown character
            Some(ch) => Err(LexError::UnexpectedCharacter(*ch)),
        };
        println!("{:?}", token);
        token
    }

    // This should be moved out into error.rs or something
    fn _unknown_char_panic(&mut self) -> ! {
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
