use char::DOMScriptChar;
use str::char_at;
use token::{ReservedWord, Token};
use std::collections::VecDeque;

#[derive(Debug)]
struct LexBuffer {
    src: String,
    // The current char being processed
    ch: Option<char>,
    // Index of the last char in the buff
    stop_index: usize,
    index: usize,
}

impl LexBuffer {
    fn new(src: String) -> LexBuffer {
        LexBuffer {
            index: 0,
            stop_index: src.len(),
            ch: char_at(&src, 0),
            src,
        }
    }
    // Eat the current character, advancing the position
    // and updatinng self.char
    fn eat(&mut self) {
        if self.index < self.stop_index {
            let next_index = self.index + 1;
            let next_ch = char_at(&self.src, next_index);
            self.ch = next_ch;
            self.index = next_index;
        }
    }

    // Peek the next n characters in the buff without eating them
    fn peek(&mut self) -> Option<char> {
        // Ensure there is a next character
        if self.index < self.stop_index - 1 {
            char_at(&self.src, self.index + 1)
        } else {
            None
        }
    }

    fn ch(&mut self) -> Option<char> {
        self.ch
    }

    pub fn read_until<F>(&mut self, predicate: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut word = String::new();
        loop {
            match self.ch {
                Some(ch) if predicate(ch) => return word,
                Some(ch) => {
                    word.push(ch);
                    self.eat();
                }
                None => return word,
            }
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    buff: LexBuffer,
    lookahead: VecDeque<Token>,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer {
            buff: LexBuffer::new(src),
            lookahead: VecDeque::with_capacity(5),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        self.buff.peek()
    }

    pub fn peek_token(&mut self) -> Option<&Token> {
        if self.lookahead.is_empty() {
            if let Some(token) = self.next_token() {
                self.lookahead.push_back(token);
            }
        }
        self.lookahead.front()
    }

    // pub fn peek2_token(&mut self) -> (Option<Token>, Option<Token>) {
    //     while self.lookahead.len() < 2 {
    //         let token = self.next_token();
    //         self.lookahead.push_back(token);
    //     }
    //     (self.lookahead)
    // }

    fn eat(&mut self) {
        self.buff.eat();
    }

    pub fn ch(&mut self) -> Option<char> {
        self.buff.ch()
    }

    fn read_line_comment(&mut self) {
        self.buff.read_until(|ch| ch.is_newline());
    }
    // Assumes self.ch is the start of an identifier
    fn read_ident(&mut self) -> String {
        self.buff.read_until(|ch| !ch.is_ident_part())
    }

    // Assumes self.ch is the opening " for the string
    fn read_string_literal(&mut self, single_quote: bool) -> String {
        // Eat the next token to start the string body
        self.eat();
        let match_ch = if single_quote { '\'' } else { '"' };
        let literal = self.buff.read_until(|ch| ch == match_ch);
        // Eat the next token to move out of the string body
        self.eat();
        literal
    }

    // Assumes self.ch is the first number in the literal
    fn read_numeric_literal(&mut self) -> i64 {
        // TODO: support floating point numbers
        self.buff
            .read_until(|ch| !ch.is_number_part())
            .parse::<i64>()
            .unwrap()
    }

    pub fn next_token(&mut self) -> Option<Token> {
        let token = match self.lookahead.pop_front() {
            Some(token) => Some(token),
            None => self.read_next_token(),
        };
        println!("next_token: {:?}", token);
        token
    }

    fn read_next_token(&mut self) -> Option<Token> {
        match self.buff.ch {
            Some(ch) => {
                match ch {
                    // Start of an identifier or reserved word
                    ch if ch.is_ident_start() => {
                        let word = self.read_ident();
                        Some(self.token_from_word(word))
                    }
                    ch if ch == '\n' => {
                        self.eat();
                        return self.next_token();
                    }
                    // Whitespace, skip it and recursively call next_token
                    ch if ch.is_whitespace() => {
                        self.eat();
                        return self.next_token();
                    }
                    '0'...'9' => Some(Token::Number(self.read_numeric_literal())),
                    // Start of a string literal
                    '"' => Some(Token::String(self.read_string_literal(false))),
                    '\'' => Some(Token::String(self.read_string_literal(true))),
                    // Single-character tokens
                    // TODO: simplify this
                    '=' => {
                        self.eat();
                        Some(Token::Assign)
                    }
                    '+' => {
                        self.eat();
                        Some(Token::Add)
                    }
                    '?' => {
                        self.eat();
                        Some(Token::TernaryCondition)
                    }
                    '(' => {
                        self.eat();
                        Some(Token::LParen)
                    }
                    ')' => {
                        self.eat();
                        Some(Token::RParen)
                    }
                    '{' => {
                        self.eat();
                        Some(Token::LCurlyBracket)
                    }
                    '}' => {
                        self.eat();
                        Some(Token::RCurlyBracket)
                    }
                    ':' => {
                        self.eat();
                        Some(Token::Colon)
                    }
                    '<' => {
                        self.eat();
                        Some(Token::LessThan)
                    }
                    '>' => {
                        // Potentially the closing symbol for an XML tag.
                        self.eat();
                        Some(Token::GreaterThan)
                    }
                    '/' => match self.peek() {
                        Some('/') => {
                            self.read_line_comment();
                            self.next_token()
                        }
                        _ => {
                            self.eat();
                            Some(Token::ForwardSlash)
                        }
                    },
                    ',' => {
                        self.eat();
                        Some(Token::Comma)
                    }
                    ';' => {
                        self.eat();
                        Some(Token::SemiColon)
                    }
                    '!' => {
                        self.eat();
                        Some(Token::Exclaim)
                    }
                    '.' => {
                        self.eat();
                        Some(Token::Dot)
                    }
                    _ => unreachable!("Unknown character: {}", ch),
                }
            }
            None => Some(Token::EOF),
        }
    }

    fn token_from_word(&mut self, word: String) -> Token {
        match word.as_ref() {
            "component" => Token::Reserved(ReservedWord::Component),
            "effect" => Token::Reserved(ReservedWord::Effect),
            "state" => Token::Reserved(ReservedWord::State),
            "type" => Token::Reserved(ReservedWord::Type),
            _ => Token::Ident(word),
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let token = self.next_token()?;
        match token {
            Token::EOF => None,
            _ => Some(token),
        }
    }
}
