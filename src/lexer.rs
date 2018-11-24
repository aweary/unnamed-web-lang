use char::DOMScriptChar;
use str::char_at;
use token::Token;

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
        if self.index < self.stop_index {
            char_at(&self.src, self.index)
        } else {
            None
        }
    }

    fn read_until<F>(&mut self, predicate: F) -> String
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

pub struct Lexer {
    buff: LexBuffer,
}

impl Lexer {
    pub fn new(src: String) -> Lexer {
        Lexer {
            buff: LexBuffer::new(src),
        }
    }

    pub fn peek(&mut self) -> Option<char> {
        // TODO
        None
    }

    fn eat(&mut self) {
        self.buff.eat();
    }

    pub fn next_token(&mut self) -> Option<Token> {
        match self.buff.ch {
            Some(ch) => {
                match ch {
                    // Start of an identifier or reserved word
                    ch if ch.is_ident_start() => {
                        let word = self.read_ident();
                        Some(self.token_from_word(word))
                    }
                    // ch if ch == '\n' => {
                    //     println!("WHITESPACE");
                    //     None
                    // },
                    // Whitespace, skipt it and recursively call next_token
                    ch if ch.is_whitespace() => {
                        self.eat();
                        return self.next_token();
                    }
                    // Start of a string literal
                    '"' => Some(Token::String(self.read_string_literal())),
                    // Single-character tokens
                    // TODO: simplify this
                    '=' => {
                        self.eat();
                        Some(Token::Assign)
                    },
                    '(' => {
                        self.eat();
                        Some(Token::LParen)
                    },
                    ')' => {
                        self.eat();
                        Some(Token::RParen)
                    },
                    '{' => {
                        self.eat();
                        Some(Token::LCurlyBracket)
                    },
                    '}' => {
                        self.eat();
                        Some(Token::RCurlyBracket)
                    },
                    ':' => {
                        self.eat();
                        Some(Token::Colon)
                    },
                    '<' => {
                        self.eat();
                        Some(Token::LCaret)
                    },
                    '>' => {
                        self.eat();
                        Some(Token::RCaret)
                    },
                    '/' => {
                        self.eat();
                        Some(Token::ForwardSlash)
                    },
                    ',' => {
                        self.eat();
                        Some(Token::Comma)
                    },
                    ';' => {
                        self.eat();
                        Some(Token::SemiColon)
                    },
                    '!' => {
                        self.eat();
                        Some(Token::Exclaim)
                    },
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

    // Assumes self.ch is the start of an identifier
    fn read_ident(&mut self) -> String {
        let mut ident = String::new();
        loop {
            match self.buff.ch {
                Some(ch) if ch.is_ident_part() => {
                    ident.push(ch);
                    self.eat();
                }
                _ => return ident,
            }
        }
    }

    // Assumes self.ch is the opening " for the string
    fn read_string_literal(&mut self) -> String {
        let mut literal = String::new();
        // Eat the next token to start the string body
        self.eat();
        loop {
            match self.buff.ch {
                Some('\"') => {
                    self.eat();
                    return literal;
                }
                Some(ch) => {
                    literal.push(ch);
                    self.eat();
                }
                None => unreachable!("read_string_literal encountered an unexpected character"),
            }
        }
    }

    fn token_from_word(&mut self, word: String) -> Token {
        match word.as_ref() {
            "component" => Token::Keyword(word),
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
