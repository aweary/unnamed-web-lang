use super::symbol_table::{SymbolTable};

use syntax::ast::Ident;
use syntax::symbol::Symbol;
use syntax::ty::Ty;

/// The shared type context.
#[derive(Default, Debug)]
pub struct TyContext {
  symbol_table: SymbolTable,
}

impl TyContext {

  pub fn resolve(&self, ident: &Ident) -> Option<&Ty> {
    self.symbol_table.resolve(&ident.name)
  }

  pub fn push_scope(&mut self) {
    self.symbol_table.push_scope();
  }

  pub fn pop_scope(&mut self) {
    self.symbol_table.pop_scope();
  }

  pub fn define(&mut self, symbol: Symbol, ty: Ty) {
    self.symbol_table.define(symbol, ty);
  }

}
