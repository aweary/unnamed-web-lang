use crate::error::ParseError;
use crate::parser::Parser;
use crate::result::Result;
use crate::token::Keyword::{Else, If, Let, Return};
use crate::token::TokenKind::{Equals, Semi};
use crate::token::TokenKind::{Keyword, LCurlyBrace, RCurlyBrace};

use super::expr::ExprParser;

use crate::ast::{Block, LetDecl, Precedence, Stmt, StmtKind};

pub trait StmtParser<'a>: ExprParser<'a> {
    fn stmt_list(&mut self) -> Result<Vec<Stmt>>;
    fn stmt(&mut self) -> Result<Stmt>;
    fn let_stmt(&mut self) -> Result<Stmt>;
    fn if_stmt(&mut self) -> Result<Stmt>;
    fn block_stmt(&mut self) -> Result<Stmt>;
    fn return_stmt(&mut self) -> Result<Stmt>;
    fn block(&mut self) -> Result<Block>;
}

impl<'a> StmtParser<'a> for Parser<'a> {
    fn stmt_list(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = vec![];
        while !self.peek_token()?.follow_stmt_list() {
            stmts.push(self.stmt()?);
            self.eat(Semi)?;
        }
        Ok(stmts)
    }

    fn stmt(&mut self) -> Result<Stmt> {
        match self.peek_token()?.kind {
            Keyword(Let) => self.let_stmt(),
            Keyword(If) => self.if_stmt(),
            Keyword(Return) => self.return_stmt(),
            LCurlyBrace => self.block_stmt(),
            _ => Err(ParseError::LexError),
        }
    }

    fn let_stmt(&mut self) -> Result<Stmt> {
        self.expect(Keyword(Let))?;
        let name = self.ident()?;
        self.expect(Equals)?;
        let init = self.expr(Precedence::NONE)?;
        let let_decl = LetDecl { name, init };
        Ok(Stmt::new(StmtKind::LetDecl(let_decl)))
    }

    fn block(&mut self) -> Result<Block> {
        self.expect(LCurlyBrace)?;
        let stmts = self.stmt_list()?;
        let block = Block(stmts);
        self.expect(RCurlyBrace)?;
        Ok(block)
    }

    fn block_stmt(&mut self) -> Result<Stmt> {
        let block = self.block()?;
        Ok(Stmt::new(StmtKind::Block(block)))
    }

    // TODO handle else if statements
    fn if_stmt(&mut self) -> Result<Stmt> {
        self.expect(Keyword(If))?;
        let condition = self.expr(Precedence::NONE)?;
        let consequent = self.block()?;
        let mut alternate = None;
        if self.eat(Keyword(Else))? {
            let alternate_block = self.block()?;
            alternate = Some(alternate_block);
        }
        Ok(Stmt::new(StmtKind::If(condition, consequent, alternate)))
    }

    fn return_stmt(&mut self) -> Result<Stmt> {
        self.expect(Keyword(Return))?;
        let expr_id = self.expr(Precedence::NONE)?;
        Ok(Stmt::new(StmtKind::Return(expr_id)))
    }
}
