use crate::ast::{Expr, ExprId, ExprKind, Template, TemplateId};
use crate::ir::{Template as TemplateIR, TemplateInstrList, StaticTemplateId};
use crate::symbol::{Symbol, SymbolTable, SymbolTableEntry, SYMBOL_DEBUG_TABLE_DEV_ONLY};
use crate::typecheck::{infer, TKind};
use id_arena::{Arena};
use string_interner::StringInterner;

use std::cell::RefCell;
use std::collections::{HashMap};

// Trait that allows structs to
pub trait ExprContext {
    fn resolve_expr(&self, id: ExprId) -> &Expr;
}

#[derive(Debug)]
struct StaticTemplateMap {
    cache: HashMap<TemplateInstrList, StaticTemplateId>,
    id: u32,
}

impl StaticTemplateMap {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
            id: 0,
        }
    }

    pub fn insert(&mut self, instrs: TemplateInstrList) -> StaticTemplateId {
        if self.cache.contains_key(&instrs) {
            *self.cache.get(&instrs).unwrap()
        } else {
            let id = StaticTemplateId(self.id);
            self.id += 1;
            self.cache.insert(instrs, id);
            id
        }
    }
}

// "Global" context used for a single parsing session
#[derive(Debug)]
pub struct ParsingContext {
    pub interner: StringInterner<Symbol>,
    // TODO make this private
    symbols: RefCell<SymbolTable>,
    expr_arena: Arena<Expr>,
    template_arena: Arena<Template>,
    type_cache: HashMap<ExprId, TKind>,
    static_template_map: RefCell<StaticTemplateMap>,
    template_ir_map: RefCell<HashMap<TemplateId, TemplateIR>>,
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
            expr_arena: Arena::with_capacity(64),
            template_arena: Arena::with_capacity(64),
            symbols: RefCell::new(SymbolTable::new()),
            type_cache: HashMap::new(),
            static_template_map: RefCell::new(StaticTemplateMap::new()),
            template_ir_map: RefCell::new(HashMap::new()),
        }
    }

    pub fn alloc_expr(&mut self, expr: Expr) -> ExprId {
        let id = self.expr_arena.alloc(expr);
        id
    }

    pub fn alloc_template(&mut self, template: Template) -> TemplateId {
        self.template_arena.alloc(template)
    }

    pub fn resolve_expr(&self, id: ExprId) -> &Expr {
        self.expr_arena.get(id).unwrap()
    }

    pub fn resolve_template(&self, id: TemplateId) -> &Template {
        self.template_arena.get(id).unwrap()
    }

    pub fn set_template_ir(&self, id: TemplateId, ir: TemplateIR) {
        self.template_ir_map.borrow_mut().insert(id, ir);
    }

    pub fn alloc_static_template_instrs(&self, instrs: TemplateInstrList) -> StaticTemplateId {
        self.static_template_map.borrow_mut().insert(instrs)
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
        // This should only happen in DEV builds
        SYMBOL_DEBUG_TABLE_DEV_ONLY.with(|symbols| {
            let mut interner = symbols.interner.borrow_mut();
            interner.get_or_intern(s);
        });
        self.interner.get_or_intern(s)
    }

    pub fn resolve_type_of(&self, sym: Symbol) -> TKind {
        match self.symbols.borrow().resolve(&sym).unwrap() {
            SymbolTableEntry::Abstract(ty) => *ty,
            SymbolTableEntry::Local(expr_id) => infer(self, *expr_id),
            _ => TKind::Error,
        }
        // Symbol table can have different kinds of entries.
        // Locals resolve to expressions which we can infer the
        // type of. Component/funciton arguments resolve directly
        // to types
    }

    // Returns a static value for an expression if it can be computed
    // at compile time.
    pub fn resolve_value_of(&self, expr_id: ExprId) -> Option<Symbol> {
        let expr = self.resolve_expr(expr_id);
        match *expr.kind {
            ExprKind::Str(text) => Some(text),
            _ => None,
        }
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

    pub fn is_defined(&self, sym: &Symbol) -> bool {
        self.symbols.borrow().resolve(sym).is_some()
    }

    pub fn type_of(&self, expr_id: ExprId) -> TKind {
        match self.type_cache.get(&expr_id) {
            Some(ty) => return *ty,
            _ => (),
        };
        infer(self, expr_id)
    }

    pub fn report_err(&self, message: &str) {
        println!("report error: {}", message)
    }
}
