use super::stmt::StmtParser;

use crate::ast::{Decl, DeclKind, FuncDecl, Param};
use crate::error::ParseError;
use crate::parser::Parser;
use crate::result::Result;
use crate::typecheck::type_of_built_in_symbol;

use crate::token::Keyword::{Func, Component};
use crate::token::TokenKind::{
    Colon, Comma, GreaterThan, Ident, Keyword, LParen, LessThan, RParen, Semi,
};

/**
 * Declarations are technically statements, but we currently
 * restrict where you can make certain kinds of declarations,
 * so its easier to separate those out.
 */
pub trait DeclParser<'a>: StmtParser<'a> {
    fn decl(&mut self) -> Result<Decl>;
    fn decl_list(&mut self) -> Result<Vec<Decl>>;
    fn fn_decl(&mut self) -> Result<FuncDecl>;
    fn fn_param(&mut self) -> Result<Param>;
    fn fn_param_list(&mut self) -> Result<Option<Vec<Param>>>;
}

impl<'a> DeclParser<'a> for Parser<'a> {
    fn decl_list(&mut self) -> Result<Vec<Decl>> {
        let mut decls = vec![];
        while !self.peek_token()?.follow_stmt_list() {
            decls.push(self.decl()?);
            self.eat(Semi)?;
        }
        Ok(decls)
    }

    fn decl(&mut self) -> Result<Decl> {
        match self.peek_token()?.kind {
            Keyword(Func) => {
                self.expect(Keyword(Func))?;
                Ok(Decl {
                kind: DeclKind::Func(self.fn_decl()?),
              })
            },
            Keyword(Component) => {
                self.expect(Keyword(Component))?;
                Ok(Decl {
                kind: DeclKind::Component(self.fn_decl()?),
            })
            },
            _ => return Err(ParseError::Unimplemented),
        }
    }

    fn fn_param(&mut self) -> Result<Param> {
        let name = self.ident()?;
        self.expect(Colon)?;
        let ident = self.type_ident()?;
        // TODO handle user-defined types
        let ty = type_of_built_in_symbol(&self.ctx, ident);
        Ok(Param { name, ty })
    }

    fn fn_param_list(&mut self) -> Result<Option<Vec<Param>>> {
        let mut params = vec![];
        loop {
            match self.peek_token()?.kind {
                RParen => {
                    self.expect(RParen)?;
                    break;
                }
                Ident(_) => {
                    let param = self.fn_param()?;
                    params.push(param);
                    if !self.eat(Comma)? {
                        self.expect(RParen)?;
                        break;
                    }
                }
                _ => return Err(ParseError::UnexpectedToken(self.next_token().unwrap())),
            }
        }
        Ok(if params.is_empty() {
            None
        } else {
            Some(params)
        })
    }

    fn fn_decl(&mut self) -> Result<FuncDecl> {
        // self.expect(Keyword(Func))?;
        let name = self.ident()?;
        // Optional type arguments
        if self.eat(LessThan)? {
            // One type argument for now
            let ty = self.type_ident()?;
            println!("ty: {:?}", ty);
            self.expect(GreaterThan)?;
        }
        self.expect(LParen)?;
        let params = self.fn_param_list()?;
        self.expect(Colon)?;
        let return_ty = self.type_ident()?;
        let block = self.block()?;
        Ok(FuncDecl {
            name,
            params,
            return_ty,
            block,
        })
    }
}
