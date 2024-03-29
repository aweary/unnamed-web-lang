use crate::reader::Reader;

use diagnostics::ParseResult as Result;

use source::diagnostics::{Diagnostic, Label};

use syntax::token::{token, Token, TokenKind};
use syntax::token::{Keyword, Lit, LitKind};
// use syntax::span::{ByteIndex, Span};
use syntax::symbol::Symbol;

use source::diagnostics::{ByteIndex, Span};

use std::collections::VecDeque;
use std::iter::Iterator;

trait IdentChar {
    fn is_id_char(&self) -> bool;
    fn is_id_start(&self) -> bool;
    fn is_id_continue(&self) -> bool;
    fn is_whitespace(&self) -> bool;
}

impl IdentChar for char {
    fn is_id_char(&self) -> bool {
        self.is_id_start() || self.is_id_continue()
    }

    fn is_id_start(&self) -> bool {
        use ucd::Codepoint;
        match *self {
            ch if Codepoint::is_id_start(ch) => true,
            ch if Codepoint::is_id_start_other(ch) => true,
            '$' | '_' => true,
            _ => false,
        }
    }

    fn is_id_continue(&self) -> bool {
        use ucd::Codepoint;
        Codepoint::is_id_continue(*self)
            || Codepoint::is_id_continue_other(*self)
    }

    fn is_whitespace(&self) -> bool {
        use ucd::Codepoint;
        Codepoint::is_whitespace(*self)
    }
}


pub struct Lexer<'a> {
    pub reader: Reader<'a>,
    pub source: &'a str,
    pub lookahead: VecDeque<Token>,
}

macro_rules! symbol {
    ($self: ident, $start: ident, $end: ident) => {
        Symbol::intern(&$self.source[($start.to_usize())..($end.to_usize())])
    };
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let reader = Reader::new(&source);
        Lexer {
            reader,
            source,
            lookahead: VecDeque::with_capacity(4),
        }
    }

    pub fn next_char(&mut self) -> Option<char> {
        self.reader.next()
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
        self.skip_while(|char| char != '\n' && char.is_whitespace());
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
            // self.sess
            //     .source_map
            //     .report_error("Expected another of the same", span, "");
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
                    // Check if this might be a range operator.
                    // If it is, bail out.
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
        use TokenKind::{Ident, Literal, Reserved};
        let span_start = self.start_span();
        let start = self.reader.offset();
        self.skip_while(|ch| ch.is_id_char());
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
                Literal(lit)
            }
            "state" => Reserved(State),
            "let" => Reserved(Let),
            "function" | "fn" | "func" => Reserved(Func),
            "component" => Reserved(Component),
            "return" => Reserved(Return),
            "if" => Reserved(If),
            "else" => Reserved(Else),
            "match" => Reserved(Match),
            "import" => Reserved(Import),
            "from" => Reserved(ImportFrom),
            "while" => Reserved(While),
            "enum" => Reserved(Enum),
            "type" => Reserved(Type),
            "struct" => Reserved(Struct),
            "for" => Reserved(For),
            "in" => Reserved(In),
            "try" => Reserved(Try),
            "catch" => Reserved(Catch),
            "const" => Reserved(Const),
            "throw" => Reserved(Throw),
            "async" => Reserved(Async),
            "await" => Reserved(Await),
            "pub" | "export" => Reserved(Pub),
            "as" => Reserved(As),
            _ => Ident(Symbol::intern(ident)),
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

    fn dot(&mut self) -> Result<Token> {
        let span_start = self.start_span();
        self.eat('.');
        let kind = if let Some('.') = self.peek_char() {
            self.eat('.');
            TokenKind::Range
        } else {
            TokenKind::Dot
        };
        let span = self.reader.end(span_start);
        Ok(token(kind, span))
    }

    fn directive(&mut self) -> Result<Token> {
        let span_start = self.reader.start();
        self.eat('%');
        self.eat('%');
        let start = self.reader.offset();
        self.skip_while(|ch| ch.is_id_char());
        let end = self.reader.offset();
        self.eat('%');
        self.eat('%');
        let span = self.reader.end(span_start);
        let kind = TokenKind::CompilerDirective(
            symbol!(self, start, end)
        );
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
        match self.peek_char() {
            Some(&ch) if ch.is_digit(10) => self.number(),
            Some(&ch) if ch.is_id_start() => self.ident(),
            Some('\n') => self.punc(Newline, '\n'),
            Some('%') => self.directive(),
            Some('"') => self.string(),
            Some('=') => self.equals(),
            Some('+') => self.plus(),
            Some('?') => self.question(),
            Some('/') => self.forward_slash(),
            Some('|') => self.pipe(),
            Some('.') => self.dot(),
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
            Some('^') => self.punc(Caret, '^'),
            Some(',') => self.punc(Comma, ','),
            Some('-') => self.punc(Minus, '-'),
            Some('*') => self.punc(Mul, '*'),
            Some(&ch) => {
                let span_start = self.start_span();
                self.eat(ch);
                let span = self.end_span(span_start);
                let label = Label::primary(span).with_message("Problem!");
                // TODO move this into a helper function
                // let diagnostic = Diagnostic::new_error(
                //     "Unexpected character",
                //     label,
                // );
                let diagnostic = Diagnostic::error()
                    .with_message("Unexpected character")
                    .with_labels(vec![label]);
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

#[cfg(test)]
mod tests {
    // ...
}
