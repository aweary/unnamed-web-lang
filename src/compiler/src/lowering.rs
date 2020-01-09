use parser::{ErrorReporting, ParserDatabase};
use salsa;
use salsa::InternKey;
use source::ModuleId;
use std::path::PathBuf;
use syntax::ast;
use syntax::symbol::Symbol;

use diagnostics::{Diagnostic, Label};

use fxhash::FxHashMap;

use crate::ir::{self, DefId, ExprId, LocalId, Reference, SpanId, StmtId};
use path_dedot::*;

type Scope = FxHashMap<Symbol, Reference>;

struct ScopeList<'a, T> {
    db: &'a T,
    scopes: Vec<Scope>,
}

impl<'a, T: IRDatabase> ScopeList<'a, T> {
    pub fn new(db: &'a T) -> Self {
        let mut scope_list = ScopeList { db, scopes: vec![] };
        // Initialize module-level scope
        scope_list.scopes.push(FxHashMap::default());
        scope_list
    }

    pub fn push_scope(&mut self) {
        let scope = FxHashMap::default();
        self.scopes.push(scope);
    }

    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn define(&mut self, name: &Symbol, item: Reference) {
        let current_scope = self
            .scopes
            .last_mut()
            .expect("Cannot define a local without a scope.");
        current_scope.insert(name.clone(), item);
    }

    pub fn resolve(&mut self, ident: &ast::Ident) -> Option<Reference> {
        let name = &ident.name.clone();
        for scope in &self.scopes {
            if scope.contains_key(&name) {
                let local = scope.get(&name).unwrap();
                return Some(local.clone());
            }
        }
        None
    }
}

// Resolve imports
pub trait ImportResolve {
    fn resolve(&self, db: &impl IRDatabase) -> PathBuf;
}

pub trait LoweringCtxt {
    fn set_current_module_id(&self, module_id: ModuleId);
    fn get_current_module_id(&self) -> ModuleId;
}

impl ImportResolve for ast::Import {
    fn resolve(&self, db: &impl IRDatabase) -> PathBuf {
        let path_buf = self.to_path_buf();
        let source_module_id = db.get_current_module_id();
        let source_path = db.lookup_intern_module_path(source_module_id);
        let source_path_dir = source_path.parent().expect("Cant resolve parent module");
        source_path_dir.join(path_buf).parse_dot().unwrap()
    }
}

pub struct LoweringContext<'a, T> {
    // Salsa database
    db: &'a T,
    scope_list: ScopeList<'a, T>,
    needs_new_scope: bool,
}

impl<'a, T> LoweringContext<'a, T>
where
    T: IRDatabase,
{
    pub fn new(db: &'a T) -> Self {
        LoweringContext {
            db,
            scope_list: ScopeList::new(db),
            needs_new_scope: true,
        }
    }

    pub fn lower_module(&mut self, id: ModuleId) -> ir::Module {
        let db = self.db;
        let ast = db.parse_module(id);
        let mut items = vec![];
        for item in ast.items {
            let id = self.lower_item(item);
            items.push(id);
        }
        ir::Module { items }
    }

    fn lower_item(&mut self, item: ast::Item) -> DefId {
        use ast::ItemKind;
        match item.kind {
            ItemKind::Fn(fn_def) => self.lower_fn(fn_def),
            ItemKind::Import(import) => self.lower_import(*import),
            _ => unreachable!("Unknown type"),
        }
    }

    fn lower_fn(&mut self, fn_def: ast::FnDef) -> DefId {
        let ast::FnDef {
            body, name, params, ..
        } = fn_def;
        self.scope_list.push_scope();
        self.needs_new_scope = false;
        for param in params.clone().into_iter() {
            let name = param.name();
            self.scope_list.define(&name, Reference::Param);
        }
        let body = self.lower_block(*body);
        let kind = ir::DefKind::Func { body, name, params };
        self.db.intern_def(ir::Def { kind })
    }

    fn lower_block(&mut self, block: ast::Block) -> ir::Block {
        let mut stmts = vec![];
        if self.needs_new_scope {
            self.scope_list.push_scope();
        } else {
            self.needs_new_scope = true;
        }
        for stmt in block.stmts {
            let stmt_id = self.lower_stmt(stmt);
            stmts.push(stmt_id);
        }
        self.scope_list.pop_scope();
        ir::Block(stmts)
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> StmtId {
        use ast::{Stmt, StmtKind};
        let Stmt { kind, .. } = stmt;
        let span = stmt.span;
        let kind = match kind {
            // Expression statement
            StmtKind::Expr(expr) => {
                let expr_id = self.lower_expr(*expr);
                ir::StmtKind::Expr(expr_id)
            }
            // Local definitions
            StmtKind::Local(local) => {
                let local_id = self.lower_local(*local);
                ir::StmtKind::Local(local_id)
            }
            // Return statement
            StmtKind::Return(expr) => {
                let expr_id = self.lower_expr(*expr);
                ir::StmtKind::Return(expr_id)
            }
            _ => {
                todo!("Unknown statemetn");
            }
        };
        self.db.intern_stmt(ir::Stmt { span, kind })
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> ExprId {
        use ast::ExprKind;
        let span = expr.span;
        let kind = match expr.kind {
            ExprKind::Unary(op, expr) => {
                let id = self.lower_expr(*expr);
                ir::ExprKind::Unary(op, id)
                // ...
            }
            ExprKind::Binary(op, left, right) => {
                let lhs = self.lower_expr(*left);
                let rhs = self.lower_expr(*right);
                ir::ExprKind::Binary { lhs, rhs, op }
            }
            ExprKind::Lit(lit) => ir::ExprKind::Literal(match lit.kind {
                ast::LitKind::Bool(symbol) => ir::Literal::Bool(symbol.into()),
                ast::LitKind::Str(symbol) => ir::Literal::String(symbol.into()),
                ast::LitKind::Number(symbol) => ir::Literal::Number(symbol.into()),
            }),
            // Reference some variable
            ExprKind::Reference(ident) => {
                // We need to resolve which
                match self.scope_list.resolve(&ident) {
                    Some(local_id) => ir::ExprKind::Reference(local_id),
                    None => {
                        let db = self.db;
                        let source_module_id = db.get_current_module_id();
                        let source_path = db.lookup_intern_module_path(source_module_id);
                        let file_id = db.resolve_file(source_path);
                        let err =
                            Diagnostic::new_error("Fuck", Label::new(file_id, span, "Unknown"));
                        db.report_diagnostic(err);
                        panic!();
                    }
                }
            }
            ExprKind::If(ast::IfExpr {
                condition, block, ..
            }) => {
                let expr_id = self.lower_expr(*condition);
                let block_id = self.lower_block(*block);
                ir::ExprKind::If(expr_id, block_id)
            }
            _ => {
                todo!("");
            }
        };
        let expr = ir::Expr { span, kind };
        self.db.intern_expr(expr)
    }

    /// When we lower a local definition we also register that name
    /// in the current scope so we can do name resolution while lowering.
    fn lower_local(&mut self, local: ast::Local) -> LocalId {
        use ast::{Local, LocalPattern};
        let Local { name, init, .. } = local;
        let init = if let Some(expr) = init {
            let expr_id = self.lower_expr(*expr);
            Some(expr_id)
        } else {
            None
        };
        // TODO need to store this in some scope map?
        let local = ir::Local { init, name: name.clone().into() };
        let id = self.db.intern_local(local);
        match name {
            LocalPattern::Ident(ident, _span) => {
                let entry = Reference::Local(id);
                self.scope_list.define(&ident.name, entry);
            }
            _ => {
                todo!("Destructuring not supported.");
            }
        };
        id
    }

    fn lower_import(&mut self, import: ast::Import) -> DefId {
        let db = self.db;
        // Get the path for the import
        // Now we need to normalize this path
        let full_path = import.resolve(self.db);
        let module_id = db.intern_module_path(full_path);
        let current_module_id = db.get_current_module_id();
        db.set_current_module_id(module_id);
        // We're parsing/lowering a new module, so we route this through
        // the query system.
        let hir = self.db.lower_module(module_id);
        db.set_current_module_id(current_module_id);
        // TODO
        DefId(0)
    }
}

pub trait LoweringT {
    fn map_id_to_span(&self, id: impl InternKey, span_id: SpanId);
    fn define(&self, ident: ast::Ident, local_id: LocalId);
}

#[salsa::query_group(IRDatabaseStorage)]
pub trait IRDatabase: ParserDatabase + LoweringT + LoweringCtxt + ErrorReporting {
    fn lower_module(&self, module_id: ModuleId) -> ir::Module;
    #[salsa::interned]
    fn intern_def(&self, def: ir::Def) -> DefId;
    #[salsa::interned]
    fn intern_local(&self, local: ir::Local) -> LocalId;
    #[salsa::interned]
    fn intern_expr(&self, expr: ir::Expr) -> ExprId;
    #[salsa::interned]
    fn intern_stmt(&self, stmt: ir::Stmt) -> StmtId;
    #[salsa::interned]
    fn intern_span(&self, span: syntax::Span) -> SpanId;
}

fn lower_module(db: &impl IRDatabase, id: ModuleId) -> ir::Module {
    let mut lowering_ctx = LoweringContext::new(db);
    lowering_ctx.lower_module(id)
}
