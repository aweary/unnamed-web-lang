use crate::reader::Reader;

use diagnostics::{Diagnostic, Label, ParseResult as Result};
use syntax::sess::ParseSess;
use syntax::token::{token, Token, TokenKind};
use syntax::token::{Keyword, Lit, LitKind};
// use syntax::span::{ByteIndex, Span};
use codespan::{ByteIndex, Span};
use syntax::symbol::Symbol;

use std::collections::VecDeque;
use std::iter::Iterator;
use ucd::Codepoint;

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum LexMode {
    Normal,
    JSX,
    TemplateText,
}

pub struct Lexer<'a> {
    pub reader: Reader<'a>,
    pub source: &'a str,
    pub sess: &'a ParseSess,
    pub mode: LexMode,
    pub lookahead: VecDeque<Token>,
}

macro_rules! symbol {
    ($self: ident, $start: ident, $end: ident) => {
        Symbol::intern(&$self.source[($start.to_usize())..($end.to_usize())])
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, sess: &'a ParseSess) -> Self {
        let reader = Reader::new(&source);
        Lexer {
            reader,
            source,
            sess,
            mode: LexMode::Normal,
            lookahead: VecDeque::with_capacity(4),
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        self.reader.next()
    }

    pub fn set_mode(&mut self, mode: LexMode) {
        self.mode = mode;
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.reader.peek()
    }

    fn start_span(&mut self) -> ByteIndex {
        self.reader.start()
    }

    fn end_span(&mut self, pos: ByteIndex) -> Span {
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
        self.skip_while(Codepoint::is_whitespace);
    }

    // Read a token containing a single character
    fn punc(&mut self, kind: TokenKind, token_char: char) -> Result<Token> {
        let span_start = self.start_span();
        self.eat(token_char);
        let span = self.end_span(span_start);
        Ok(token(kind, span))
    }

    fn punc2(&mut self, kind: TokenKind, ch: char) -> Result<Token> {
        let span_start = self.start_span();
        self.eat(ch);
        if self.peek_char() == Some(&ch) {
            self.eat(ch);
            let span = self.end_span(span_start);
            Ok(token(kind, span))
        } else {
            let _ch2 = self.next_char().unwrap();
            let span = self.end_span(span_start);
            self.sess
                .source_map
                .report_error("Expected another of the same", span, "");
            // TODO how to handle reporting a sequence of lex errors?
            Ok(token(TokenKind::LexError, span))
            // Err(Error::UnexpectedCharacter(ch2))
        }
    }

    /**
     * Tokenize a number literal. Supports both integers and floats,
     * which are represented the same way in the language anyways.
     */
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
        let span = self.end_span(span_start);
        let literal = Lit {
            kind: LitKind::Number,
            symbol: symbol!(self, start, end),
        };
        Ok(token(TokenKind::Literal(literal), span))
    }

    fn eof(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        let span = self.end_span(span_start);
        Ok(token(TokenKind::EOF, span))
    }

    /**
     * Parses a single word as either a user-defined identifier
     * or a reserved word (keyword).
     */
    fn ident(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        let start = self.reader.offset();
        self.skip_while(|ch| Codepoint::is_id_start(ch) || Codepoint::is_id_continue(ch));
        let end = self.reader.offset();
        let span = self.reader.end(span_start);
        let ident = &self.source[start.to_usize()..end.to_usize()];
        use Keyword::*;
        let kind = match ident {
            "true" | "false" => {
                let lit = Lit {
                    kind: LitKind::Bool,
                    symbol: Symbol::intern(ident),
                };
                TokenKind::Literal(lit)
            }
            "let" | "state" => TokenKind::Reserved(Let),
            "function" | "fn" | "func" => TokenKind::Reserved(Func),
            "component" => TokenKind::Reserved(Component),
            "return" => TokenKind::Reserved(Return),
            "if" => TokenKind::Reserved(If),
            "else" => TokenKind::Reserved(Else),
            "match" => TokenKind::Reserved(Match),
            "import" => TokenKind::Reserved(Import),
            "from" => TokenKind::Reserved(ImportFrom),
            "while" => TokenKind::Reserved(While),
            "enum" => TokenKind::Reserved(Enum),
            "type" => TokenKind::Reserved(Type),
            "for" => TokenKind::Reserved(For),
            "in" => TokenKind::Reserved(In),
            "try" => TokenKind::Reserved(Try),
            "catch" => TokenKind::Reserved(Catch),
            _ => TokenKind::Ident(Symbol::intern(ident)),
        };
        Ok(token(kind, span))
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
        Ok(token(kind, span))
    }

    fn string(&mut self) -> Result<Token> {
        let span_start = self.reader.start();
        let start = self.reader.offset();
        self.eat('"');
        self.skip_while(|ch| ch != '"');
        self.eat('"');
        let end = self.reader.offset();
        let span = self.reader.end(span_start);
        let lit = Lit {
            kind: LitKind::Str,
            symbol: symbol!(self, start, end),
        };
        Ok(token(TokenKind::Literal(lit), span))
    }

    fn plus(&mut self) -> Result<Token> {
        let span_start = self.reader.start();
        self.eat('+');
        let kind = match self.peek_char() {
            Some('=') => {
                self.eat('=');
                TokenKind::PlusEquals
            }
            _ => TokenKind::Plus,
        };
        let span = self.reader.end(span_start);
        Ok(token(kind, span))
    }

    fn forward_slash(&mut self) -> Result<Token> {
        let token = self.punc(TokenKind::Div, '/');
        match self.peek_char() {
            Some('/') => {
                self.skip_while(|ch| ch != '\n');
                self.next_token()
            }
            _ => token,
        }
    }

    fn question(&mut self) -> Result<Token> {
        let span_start = self.reader.start();
        self.eat('?');
        let kind = match self.peek_char() {
            // This is sensitive to whitespace, so make sure we
            // avoid skip_whitespace
            Some('.') => {
                self.eat('.');
                TokenKind::QuestionDot
            }
            _ => TokenKind::Question,
        };
        let span = self.reader.end(span_start);
        Ok(token(kind, span))
    }

    fn pipe(&mut self) -> Result<Token> {
        let lo = self.reader.start();
        self.eat('|');
        let kind = match self.peek_char() {
            Some('>') => {
                self.eat('>');
                TokenKind::Pipeline
            }
            Some('|') => {
                self.eat('|');
                TokenKind::Or
            }
            _ => TokenKind::BinOr,
        };
        let span = self.reader.end(lo);
        Ok(token(kind, span))
    }

    pub fn next_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        // Read from the lookahead if its populated.
        if let Some(token) = self.lookahead.pop_front() {
            return Ok(token);
        }
        // Ignore all whitespace.
        self.skip_whitespace();
        if self.mode == LexMode::TemplateText {
            return self.next_jsx_token();
        }
        // Skip comments
        match self.peek_char() {
            Some(&ch) if ch.is_digit(10) => self.number(),
            Some(&ch) if Codepoint::is_id_start(ch) => self.ident(),
            Some('"') => self.string(),
            Some('=') => self.equals(),
            Some('+') => self.plus(),
            Some('?') => self.question(),
            Some('/') => self.forward_slash(),
            Some('|') => self.pipe(),
            Some('&') => self.punc2(And, '&'),
            Some('!') => self.punc(Exclaim, '!'),
            Some('(') => self.punc(LParen, '('),
            Some(')') => self.punc(RParen, ')'),
            Some('{') => self.punc(LCurlyBrace, '{'),
            Some('}') => self.punc(RCurlyBrace, '}'),
            Some('[') => self.punc(LBrace, '['),
            Some(']') => self.punc(RBrace, ']'),
            Some('<') => self.punc(LessThan, '<'),
            Some('>') => self.punc(GreaterThan, '>'),
            Some(':') => self.punc(Colon, ':'),
            Some(';') => self.punc(Semi, ';'),
            Some('.') => self.punc(Dot, '.'),
            Some('%') => self.punc(Mod, '%'),
            Some('^') => self.punc(Caret, '^'),
            Some(',') => self.punc(Comma, ','),
            Some('-') => self.punc(Minus, '-'),
            Some('*') => self.punc(Mul, '*'),
            Some(&ch) => {
                println!("unexpected char {:?}", ch);
                let span_start = self.start_span();
                self.eat(ch);
                let span = self.end_span(span_start);
                // TODO move this into a helper function
                let diagnostic = Diagnostic::new_error(
                    "Unexpected character",
                    Label::new(self.sess.source_map.current_file_id(), span, ""),
                );
                Err(diagnostic)
            }
            _ => self.eof(),
        }
    }

    pub fn peek_token(&mut self) -> Result<&Token> {
        if self.lookahead.is_empty() {
            let token = self.next_token()?;
            self.lookahead.push_front(token);
        }
        Ok(self.lookahead.front().unwrap())
    }
}

// Seperate code path for lexing tokens in TemplateText mode. This is an easy way
// to handle the different lexing semantics for unknown characters inside JSX elements.

impl Lexer<'_> {
    fn next_jsx_token(&mut self) -> Result<Token> {
        use TokenKind::*;
        match self.peek_char() {
            Some('<') => self.punc(LessThan, '<'),
            Some('>') => self.punc(GreaterThan, '>'),
            Some('{') => self.punc(LCurlyBrace, '{'),
            Some('}') => self.punc(RCurlyBrace, '}'),
            _ => self.jsx_text(),
        }
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
            let symbol = symbol!(self, start, end);
            let kind = TokenKind::TemplateText(symbol);
            Ok(token(kind, span))
        }
    }
}

#[cfg(test)]
mod tests {
    // ...
}
