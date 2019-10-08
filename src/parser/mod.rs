mod decl;
mod expr;
mod stmt;
mod context;

use decl::DeclParser;
// use expr::ExprParser;
// use stmt::StmtParser;
pub use context::{ParsingContext, ExprContext};


use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::result::Result;
use crate::ast::{Module, Program};
use crate::symbol::Symbol;
use crate::token::{Token, TokenKind};
use std::rc::Rc;
use std::cell::RefCell;

// temporary use so that the Rust compiler checks this module
// use crate::ir::*;

pub fn parse_program<'a>(source: &'a str, mut ctx: &'a mut ParsingContext) -> Result<Program> {
    let mut parser = Parser::new(&source, &mut ctx);
    parser.parse()
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    ctx: &'a mut ParsingContext,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, ctx: &'a mut ParsingContext) -> Self {
        let lexer = Lexer::new(&source);
        Parser { lexer, ctx }
    }

    fn next_token(&mut self) -> Result<Token> {
        match self.lexer.next_token(&mut self.ctx) {
            Ok(token) => {
                Ok(token)
            }
            Err(_) => Err(ParseError::LexError),
        }
    }

    fn peek_token(&mut self) -> Result<&Token> {
        match self.lexer.peek_token(&mut self.ctx) {
            Ok(token) => Ok(token),
            Err(_) => Err(ParseError::LexError),
        }
    }

    fn eat(&mut self, kind: TokenKind) -> Result<bool> {
        if self.peek_token()?.kind == kind {
            self.expect(kind)?;
            Ok(true)
        } else {
            Ok(false)
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

    /**
     * This is currently an alias to `ident`, but a type
     * identifier has different syntax. For example, polymorphic
     * types have type parameters.
     */
    fn type_ident(&mut self) -> Result<Symbol> {
        self.ident()
    }

    pub fn parse(&mut self) -> Result<Program> {
        // top-level statements
        let stmts = self.decl_list()?;
        let module = Module { stmts };
        let modules = vec![module];
        let program = Program { modules };
        Ok(program)
    }
}
