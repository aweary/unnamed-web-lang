use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::num::NonZeroU32;
use string_interner::StringInterner;
pub use string_interner::Symbol as SymbolTrait;

use crate::ast::ExprId;
use crate::typecheck::TKind;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SymbolTableEntry {
    Local(ExprId),
    Abstract(TKind),
}

impl SymbolTrait for Symbol {
    fn from_usize(val: usize) -> Self {
        Symbol(NonZeroU32::new((val + 1) as u32).unwrap_or_else(|| {
            unreachable!("Should never fail because `val + 1` is nonzero and `<= u32::MAX`")
        }))
    }
    fn to_usize(self) -> usize {
        (self.0.get() as usize) - 1
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // For non-release builds we use
        if cfg!(debug_assertions) {
            SYMBOL_DEBUG_TABLE_DEV_ONLY.with(|symbols| {
                let interner = symbols.interner.borrow();
                let string = interner.resolve(*self).unwrap();
                write!(f, "\"s#{}\"", string)
            })
        } else {
            write!(f, "Symbol({})", self.0)
        }
    }
}

#[derive(Debug)]
pub struct Symbols {
    pub interner: RefCell<StringInterner<Symbol>>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            interner: RefCell::new(StringInterner::new()),
        }
    }
}

// TODO make this DEV-only
#[cfg(debug_assertions)]
thread_local!(pub static SYMBOL_DEBUG_TABLE_DEV_ONLY: Symbols = Symbols::new());

#[derive(Debug)]
pub struct SymbolTable {
    pub scope_count: u32,
    scopes: Vec<HashMap<Symbol, SymbolTableEntry>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        let scopes = vec![HashMap::new()];
        Self {
            scope_count: 0,
            scopes,
        }
    }

    pub fn begin_scope(&mut self) {
        let scope = HashMap::new();
        self.scopes.push(scope);
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define_local(&mut self, symbol: Symbol, expr_id: ExprId) {
        let entry = SymbolTableEntry::Local(expr_id);
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(symbol, entry);
    }

    pub fn define_abstract(&mut self, symbol: Symbol, ty: TKind) {
        let entry = SymbolTableEntry::Abstract(ty);
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(symbol, entry);
    }

    pub fn resolve(&self, sym: &Symbol) -> Option<&SymbolTableEntry> {
        // Walk up the scope chain
        for scope in &self.scopes {
            if scope.contains_key(sym) {
                return scope.get(sym);
            }
        }
        None
    }
}
