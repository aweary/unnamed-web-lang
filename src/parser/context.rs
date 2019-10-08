use string_interner::{StringInterner};
use id_arena::{Arena};
use crate::ast::{Expr, ExprId};
use crate::symbol::{SymbolTable, Symbol, SymbolTableEntry};
use crate::typecheck::{TKind, infer};

use std::cell::RefCell;
use std::collections::HashMap;


// Trait that allows structs to 
pub trait ExprContext {
    fn resolve_expr(&self, id: ExprId) -> &Expr; 
}

// "Global" context used for a single parsing session
#[derive(Debug)]
pub struct ParsingContext {
    pub interner: StringInterner<Symbol>,
    // TODO make this private
    symbols: RefCell<SymbolTable>,
    expr_arena: Arena<Expr>,
    type_cache: HashMap<ExprId, TKind>,
}

impl ExprContext for ParsingContext {
    fn resolve_expr(&self, id: ExprId) -> &Expr {
        self.expr_arena.get(id).unwrap()
    }
}

impl ParsingContext {
    pub fn new() -> Self {
        Self {
            interner: StringInterner::new(),
            expr_arena: Arena::with_capacity(256),
            symbols: RefCell::new(SymbolTable::new()),
            type_cache: HashMap::new(),
        }
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        let id = self.expr_arena.alloc(expr);
        id
    }
    
    pub fn resolve_expr(&self, id: ExprId) -> &Expr {
        self.expr_arena.get(id).unwrap()
    }

    pub fn has_ty_for(&self, id: ExprId) -> bool {
        self.type_cache.contains_key(&id) 
    }

    pub fn begin_scope(&self) {
        self.symbols.borrow_mut().begin_scope();
    }
    
    pub fn end_scope(&self) {
        self.symbols.borrow_mut().end_scope();
    }

    pub fn symbol_str(&self, sym: Symbol) -> Option<&str> {
        self.interner.resolve(sym)
    }

    pub fn symbol(&mut self, s: &str) -> Symbol {
        self.interner.get_or_intern(s)
    }
 
    pub fn resolve_type_of(&self, sym: Symbol) -> TKind {
        println!("resolve type");
        match self.symbols.borrow().resolve(&sym).unwrap() {
            SymbolTableEntry::Abstract(ty) => *ty,
            SymbolTableEntry::Local(expr_id) => {
                infer(self, *expr_id)
            }
            _ => TKind::Error,
        }
        // Symbol table can have different kinds of entries.
        // Locals resolve to expressions which we can infer the
        // type of. Component/funciton arguments resolve directly
        // to types
    }

    // pub fn resolve_symbol(&self, sym: Symbol) -> ExprId {
    //     println!("resolving symbol {:?}", sym);
    //     match self.symbols.borrow().resolve(&sym) {
    //         Some(expr_id) => expr_id,
    //         None => {
    //         }
    //     }
    // }

    pub fn define_local(&self, sym: Symbol, expr_id: ExprId) {
        self.symbols.borrow_mut().define_local(sym, expr_id);
    }

    pub fn define_abstract(&self, sym: Symbol, ty: TKind) {
        self.symbols.borrow_mut().define_abstract(sym, ty);
    }

    pub fn type_of(&self, expr_id: ExprId) -> TKind {
        match self.type_cache.get(&expr_id) {
            Some(ty) => return *ty,
            _ => ()
        };
        println!("asking typeof {:?}", expr_id);
        infer(self, expr_id)
    }

    pub fn report_err(&self, message: &str) {
        println!("report error: {}", message)
    }
}
