mod expr;
mod stmt;
mod result;

use result::Result;

// Parsing methods are split among a handful of imported traits
use expr::ExprParser;
use stmt::StmtParser;

use bumpalo::Bump;

use generational_arena::{Arena, Index};

use crate::error::ParseError;
use crate::lexer::Lexer;
// use crate::symbols::ScopedSymbolTable;
use crate::context::ParsingContext;
use crate::token::{Token, TokenKind};
use crate::ast::{Precedence, ExprKind};
use crate::symbol::Symbol;

// use arena::{Arena, Id};

pub fn parse_module<'a>(source: &'a str) -> String {
    let arena = Bump::new();
    let mut parser = Parser::new(&source, &arena);
    match parser.parse() {
        Ok(results) => results,
        Err(err) => {
            println!("err: {:?}", err);
            // TODO error reporting. We can't return any error data that is
            // using this memory arena
            String::from("Something failed")
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    expr_table: Arena<ExprKind>,
    ctx: ParsingContext,
    arena: &'a Bump,
    // symbols: ScopedSymbolTable<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, arena: &'a Bump) -> Self {
        let lexer = Lexer::new(&source);
        Parser {
            lexer,
            expr_table: Arena::new(),
            ctx: ParsingContext::new(),
            arena: &arena,
        }
    }

    fn next_token(&mut self) -> Result<Token> {
        match self.lexer.next_token() {
            Ok(token) => Ok(token),
            Err(_) => Err(ParseError::LexError),
        }
    }

    fn peek_token(&mut self) -> Result<&Token> {
        match self.lexer.peek_token() {
            Ok(token) => Ok(token),
            Err(_) => Err(ParseError::LexError)
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.next_token()?;
        if token.kind != kind {
            Err(ParseError::UnexpectedToken(token))
        } else {
            Ok(token)
        }
    }

    fn ident(&mut self) -> Result<Symbol> {
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Ident(sym) => Ok(sym),
            _ => Err(ParseError::UnexpectedToken(token)),
        }
    }

    pub fn parse(&mut self) -> Result<String> {
        let stmt = self.stmt_list();
        println!("expr {:#?}", stmt);
        let compiled_program = String::from("fake compiled program");
        Ok(compiled_program)
    }
}
