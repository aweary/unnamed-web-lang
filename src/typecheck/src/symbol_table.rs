use super::Ty;

use std::collections::HashMap;
use syntax::ast::*;
use syntax::symbol::Symbol;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SymbolTableEntry {}

#[derive(Debug)]
pub struct SymbolTable {
    scopes: Vec<HashMap<Symbol, Ty>>,
}

impl Default for SymbolTable {
    fn default() -> SymbolTable {
        let scopes = vec![
            // Initiaize with the global/module scope
            HashMap::new(),
        ];
        SymbolTable { scopes }
    }
}

impl SymbolTable {
    pub fn push_scope(&mut self) {
        let scope = HashMap::new();
        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, symbol: Symbol, ty: Ty) {
        println!("define");
        // TODO better None hndling
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(symbol, ty);
        // TODO clone shouldnt be required once symbols are interned
        // scope.insert(ident.name.clone());
    }

    /// Resolve the type for some reference.
    pub fn resolve(&self, symbol: &Symbol) -> Option<&Ty> {
        for scope in &self.scopes {
            if scope.contains_key(symbol) {
                return scope.get(symbol)
            }
        }
        None
    }
}
