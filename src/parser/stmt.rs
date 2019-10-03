
use super::result::Result;

use crate::error::ParseError;
use crate::parser::Parser;
use crate::token::{TokenKind, Keyword};


use super::expr::ExprParser;

use crate::ast::{Stmt, StmtKind, Precedence};

pub trait StmtParser<'a> : ExprParser<'a> {
  fn stmt_list(&mut self) -> Result<Vec<Stmt>>;
  fn stmt(&mut self) -> Result<Stmt>;
  fn let_stmt(&mut self) -> Result<Stmt>;
}

impl<'a> StmtParser<'a> for Parser<'a> {

  fn stmt_list(&mut self) -> Result<Vec<Stmt>> {
    let mut stmts = vec![];
    while !self.peek_token()?.follow_stmt_list() {
      stmts.push(self.stmt()?);
      if self.peek_token()?.kind == TokenKind::Semi {
        self.next_token()?;
      }
    }
    Ok(stmts)
  }

  fn stmt(&mut self) -> Result<Stmt> {
    match self.peek_token()?.kind {
      TokenKind::Keyword(Keyword::Let) => self.let_stmt(),
      _ => Err(ParseError::LexError)
    }
  }

  fn let_stmt(&mut self) -> Result<Stmt> {
    self.expect(TokenKind::Keyword(Keyword::Let))?;
    let name = self.ident()?;
    self.expect(TokenKind::Equals)?;
    let val = self.expr(Precedence::NONE)?;
    Ok(Stmt::new(
      StmtKind::LetDecl(name, Box::new(val)) 
    ))
  }
}