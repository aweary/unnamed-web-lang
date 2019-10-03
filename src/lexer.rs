use ucd::Codepoint;

use crate::error::LexError;
use crate::pos::{Pos, Span};
use crate::reader::Reader;
use crate::token::{Keyword, Token, TokenKind};

use crate::symbol::symbol;

use std::collections::VecDeque;
use std::iter::Iterator;

type Result<T> = std::result::Result<T, LexError>;

pub enum LexMode {
    Normal,
    JSX,
    JSXText,
}

pub struct Lexer<'a> {
    pub reader: Reader<'a>,
    pub source: &'a str,
    pub mode: LexMode,
    pub lookahead: VecDeque<Token>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let reader = Reader::new(&source);
        Lexer {
            reader,
            source,
            mode: LexMode::Normal,
            lookahead: VecDeque::with_capacity(4),
        }
    }

    fn next_char(&mut self) -> Option<char> {
        self.reader.next()
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.reader.peek()
    }

    fn start_span(&mut self) -> Pos {
        self.reader.start()
    }

    fn end_span(&mut self, pos: Pos) -> Span {
        self.reader.end(pos)
    }

    fn eat(&mut self, ch: char) {
        let next_ch = self.next_char().expect("eat() called on empty reader");
        debug_assert_eq!(ch, next_ch);
    }

    fn skip_while<F>(&mut self, pred: F)
    where
        F: Fn(char) -> bool,
    {
        loop {
            match self.peek_char() {
                Some(&ch) if pred(ch) => {
                    self.eat(ch);
                }
                _ => return,
            }
        }
    }

    fn skip_whitespace(&mut self) {
        self.skip_while(|ch| Codepoint::is_whitespace(ch));
    }

    // Read a token containing a single character
    fn single(&mut self, kind: TokenKind, token_char: char) -> Result<Token> {
        let span_start = self.start_span();
        self.eat(token_char);
        let span = self.end_span(span_start);
        Ok(Token::new(kind, span))
    }

    fn double(&mut self, kind: TokenKind, ch: char) -> Result<Token> {
        let span_start = self.start_span();
        self.eat(ch);
        if self.peek_char() == Some(&ch) {
            self.eat(ch);
            let span = self.end_span(span_start);
            Ok(Token::new(kind, span))
        } else {
            Err(LexError::UnexpectedCharacter(self.next_char().unwrap()))
        }
    }

    fn number(&mut self) -> Result<Token> {
        let mut seen_decimal = false;
        let span_start = self.start_span();
        let start = self.reader.offset();
        loop {
            match self.peek_char() {
                Some('.') => {
                    if seen_decimal {
                        break;
                    }
                    self.eat('.');
                    seen_decimal = true;
                }
                Some(&ch) if ch.is_digit(10) => {
                    self.eat(ch);
                }
                _ => {
                    break;
                }
            }
        }
        let end = self.reader.offset();
        let num = self.source[start..end].parse::<f64>().unwrap();
        let span = self.end_span(span_start);
        Ok(Token::new(TokenKind::Number(num), span))
    }

    fn eof(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        let span = self.end_span(span_start);
        Ok(Token::new(TokenKind::EOF, span))
    }

    fn ident(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        let start = self.reader.offset();
        self.skip_while(|ch| Codepoint::is_id_start(ch) || Codepoint::is_id_continue(ch));
        let end = self.reader.offset();
        let span = self.reader.end(span_start);
        let ident = &self.source[start..end];
        let kind = match ident {
            "let" | "state" => TokenKind::Keyword(Keyword::Let),
            "function" | "fn" | "component" => TokenKind::Keyword(Keyword::Func),
            "return" => TokenKind::Keyword(Keyword::Return),
            "if" => TokenKind::Keyword(Keyword::If),
            "else" => TokenKind::Keyword(Keyword::Else),
            "match" => TokenKind::Keyword(Keyword::Match),
            "import" => TokenKind::Keyword(Keyword::Import),
            "from" => TokenKind::Keyword(Keyword::ImportFrom),
            _ => {
                let symbol = symbol(ident);
                TokenKind::Ident(symbol)
            }
        };
        Ok(Token::new(kind, span))
    }

    fn equals(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        self.eat('=');
        let kind = match self.peek_char() {
            Some('=') => {
                self.eat('=');
                TokenKind::DblEquals
            }
            Some('>') => {
                self.eat('>');
                TokenKind::Arrow
            }
            _ => TokenKind::Equals,
        };
        let span = self.reader.end(span_start);
        Ok(Token::new(kind, span))
    }

    fn string(&mut self) -> Result<Token> {
        let span_start = self.reader.start();
        let start = self.reader.offset();
        self.eat('"');
        self.skip_while(|ch| ch != '"');
        self.eat('"');
        let end = self.reader.offset();
        let span = self.reader.end(span_start);
        let sym = symbol(&self.source[start..end]);
        Ok(Token::new(TokenKind::String(sym), span))
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        if let Some(token) = self.lookahead.pop_front() {
            return Ok(token);
        }
        self.skip_whitespace();
        let token = match self.peek_char() {
            Some(&ch) if ch.is_digit(10) => self.number(),
            Some(&ch) if Codepoint::is_id_start(ch) => self.ident(),
            Some('"') => self.string(),
            Some('=') => self.equals(),
            Some('|') => self.double(Or, '|'),
            Some('&') => self.double(And, '&'),
            Some('(') => self.single(LParen, '('),
            Some(')') => self.single(RParen, ')'),
            Some('{') => self.single(LBrace, '{'),
            Some('}') => self.single(RBrace, '}'),
            Some('<') => self.single(LessThan, '<'),
            Some('>') => self.single(GreaterThan, '>'),
            Some(':') => self.single(Colon, ':'),
            Some(';') => self.single(Semi, ';'),
            Some('.') => self.single(Dot, '.'),
            Some('%') => self.single(Mod, '%'),
            Some('^') => self.single(Caret, '^'),
            Some('?') => self.single(Question, '?'),
            Some(',') => self.single(Comma, ','),
            Some('+') => self.single(Plus, '+'),
            Some('-') => self.single(Minus, '-'),
            Some('*') => self.single(Mul, '*'),
            Some('/') => self.single(Div, '/'),
            Some(&ch) => Err(LexError::UnexpectedCharacter(ch)),
            _ => self.eof(),
        };
        // println!("Token: {:?}", token);
        token
    }

    pub fn peek_token(&mut self) -> Result<&Token> {
        if self.lookahead.is_empty() {
            let token = self.next_token()?;
            self.lookahead.push_front(token);
        }
        Ok(self.lookahead.front().unwrap())
    }
}

// Seperate code path for lexing tokens in JSXText mode. This is an easy way
// to handle the different lexing semantics for unknown characters inside JSX elements.
trait JSXLexer<'a> {
    fn next_jsx_token(&mut self) -> Result<Token>;
    fn jsx_text(&mut self) -> Result<Token>;
}

impl<'a> JSXLexer<'a> for Lexer<'a> {
    fn next_jsx_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        let token = match self.peek_char() {
            Some('<') => self.single(LessThan, '<'),
            Some('>') => self.single(GreaterThan, '>'),
            Some('{') => self.single(LBrace, '{'),
            Some('}') => self.single(RBrace, '}'),
            _ => self.jsx_text(),
        };
        token
    }

    fn jsx_text(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        let start = self.reader.offset();
        self.skip_while(|ch| match ch {
            '}' | '{' | '<' | '>' => false,
            _ => true,
        });
        let end = self.reader.offset();
        if start == end {
            self.next_token()
        } else {
            let span = self.end_span(span_start);
            let sym = symbol(&self.source[start..end]);
            let kind = TokenKind::JSXText(sym);
            Ok(Token::new(kind, span))
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => match token.kind {
                TokenKind::EOF => None,
                _ => Some(token),
            },
            _ => None,
        }
    }
}
