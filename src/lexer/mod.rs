use std::collections::VecDeque;
use std::iter::Iterator;

pub mod error;

use colored::*;
use ucd::Codepoint;

use crate::reader::Reader;
use crate::token::{Keyword, Token, TokenKind};

use error::LexError;

type Result<T> = std::result::Result<T, LexError>;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum LexMode {
    Normal = 0,
    JSX = 1,
    JSXText = 2,
}

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a String,
    chars: Reader<'a>,
    lookahead: VecDeque<Token>,
    pub mode: LexMode,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a String) -> Self {
        let chars = source.chars();
        Lexer {
            source,
            chars: Reader::new(chars),
            lookahead: VecDeque::with_capacity(4),
            mode: LexMode::Normal,
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
        let span = self.chars.end(span_start);
        let token = Token::new(kind, span);
        Ok(token)
    }

    fn read_equals(&mut self) -> Result<Token> {
        let span_start = self.chars.start();
        // Eat..
        let eat = self.chars.next();
        match self.chars.peek() {
            Some('=') => {
                self.chars.next();
                let span = self.chars.end(span_start);
                let token = Token::new(TokenKind::DblEquals, span);
                Ok(token)
            }
            Some('>') => {
                self.chars.next();
                let span = self.chars.end(span_start);
                let token = Token::new(TokenKind::Arrow, span);
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
            "function" | "fn" | "component" => TokenKind::Keyword(Keyword::Func),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "match" => TokenKind::Keyword(Keyword::Match),
            _ => TokenKind::Ident(ident),
        };
        let token = Token::new(kind, span);
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
        println!("txt {}", text.is_empty());
        let span = self.chars.end(start);
        // Dont output JSXText token if theres no text
        if text.is_empty() {
            self.next_token()
        } else {
            Ok(Token::new(TokenKind::JSXText(text), span))
        }
    }

    // Seperate code path for lexing tokens in JSXText mode. This is an easy way
    // to handle the different lexing semantics for unknown characters inside JSX elements.
    fn next_jsx_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        let token = match self.chars.peek() {
            Some('<') => self.read_punc(LessThan),
            Some('>') => self.read_punc(GreaterThan),
            Some('{') => self.read_punc(LBrace),
            Some('}') => self.read_punc(RBrace),
            _ => self.read_jsx_text(),
        };
        println!("next JSX token {:?}", token);
        token
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        self.skip_whitespace();
        if self.mode == LexMode::JSXText {
            return self.next_jsx_token();
        }
        let token = match self.chars.peek() {
            // Some(ch)
            //     if self.mode == LexMode::JSXText
            //         && *ch != '{'
            //         && *ch != '}'
            //         && *ch != '<'
            //         && *ch != '>' =>
            // {
            //     let token = self.read_jsx_text()?;
            //     Ok(token)
            // }
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
        println!("{:?}: {:?}", self.mode, token);
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
