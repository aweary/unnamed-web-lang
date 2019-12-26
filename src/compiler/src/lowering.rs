use parser::ParserDatabase;
use salsa;
use salsa::InternKey;

use source::ModuleId;
use syntax::ast;

use std::path::PathBuf;

use crate::ir::{self, DefId, ExprId, LocalId, SpanId, StmtId};

use id_arena::{Arena, Id};

use path_dedot::*;

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
}

impl<'a, T> LoweringContext<'a, T>
where
    T: IRDatabase,
{
    pub fn new(db: &'a T) -> Self {
        LoweringContext { db }
    }

    pub fn lower_module(&mut self, id: ModuleId) -> ir::Module {
        let db = self.db;
        let ast = db.parse_module(id);
        let mut items = vec![];
        for item in ast.items {
            let id = db.lower_item(item);
            items.push(id);
        }
        ir::Module { items }
    }
}

pub trait LoweringT {
    fn map_id_to_span(&self, id: impl InternKey, span_id: SpanId);
    fn define(&self, ident: ast::Ident, local_id: LocalId);
}

#[salsa::query_group(IRDatabaseStorage)]
pub trait IRDatabase: ParserDatabase + LoweringT + LoweringCtxt {
    #[salsa::input]
    fn module_id(&self, key: ()) -> ModuleId;
    fn lower_module(&self, module_id: ModuleId) -> ir::Module;
    fn lower_item(&self, item: ast::Item) -> DefId;
    fn lower_expr(&self, expr: ast::Expr) -> ExprId;
    fn lower_fn(&self, fn_def: ast::FnDef) -> DefId;
    fn lower_import(&self, fn_def: ast::Import) -> DefId;
    fn lower_stmt(&self, stmt: ast::Stmt) -> StmtId;
    fn lower_block(&self, block: ast::Block) -> ir::Block;
    fn lower_local(&self, local: ast::Local) -> LocalId;
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

/// Resolve an import and get a reference to the DefId for the import(s)
/// that this import is using.
fn lower_import(db: &impl IRDatabase, import: ast::Import) -> DefId {
    // Get the path for the import
    // Now we need to normalize this path
    let full_path = import.resolve(db);
    let module_id = db.intern_module_path(full_path);
    let hir = db.lower_module(module_id);
    println!("lowering import {:?}", hir);
    DefId(0)
}

fn lower_module(db: &impl IRDatabase, id: ModuleId) -> ir::Module {
    let mut lowering_ctx = LoweringContext::new(db);
    lowering_ctx.lower_module(id)
}

fn lower_item(db: &impl IRDatabase, item: ast::Item) -> DefId {
    use ast::ItemKind;
    match item.kind {
        ItemKind::Fn(fn_def) => db.lower_fn(fn_def),
        ItemKind::Import(import) => db.lower_import(*import),
        _ => unreachable!("Unknown type"),
    }
}

fn lower_fn(db: &impl IRDatabase, fn_def: ast::FnDef) -> DefId {
    let ast::FnDef { body, .. } = fn_def;
    let body = db.lower_block(*body);
    let kind = ir::DefKind::Func { body };
    db.intern_def(ir::Def { kind })
}

fn lower_stmt(db: &impl IRDatabase, stmt: ast::Stmt) -> StmtId {
    use ast::{Stmt, StmtKind};
    let Stmt { kind, .. } = stmt;
    db.intern_stmt(match kind {
        // Expression statement
        StmtKind::Expr(expr) => {
            let expr_id = db.lower_expr(*expr);
            ir::Stmt::Expr(expr_id)
        }
        // Local definitions
        StmtKind::Local(local) => {
            let local_id = db.lower_local(*local);
            ir::Stmt::Local(local_id)
        }
        _ => {
            todo!("Unknown statemetn");
        }
    })
}

/// When we lower a local definition we also register that name
/// in the current scope so we can do name resolution while lowering.
fn lower_local(db: &impl IRDatabase, local: ast::Local) -> LocalId {
    use ast::{Local, LocalPattern};
    let Local { name, init, .. } = local;
    let init = if let Some(expr) = init {
        let expr_id = db.lower_expr(*expr);
        Some(expr_id)
    } else {
        None
    };
    // TODO need to store this in some scope map?
    let local = ir::Local { init };
    let id = db.intern_local(local);
    match name {
        LocalPattern::Ident(ident, _span) => {
            db.define(ident, id);
            // ...
        }
        _ => {
            todo!("Destructuring not supported.");
        }
    };
    id
}

fn lower_block(db: &impl IRDatabase, block: ast::Block) -> ir::Block {
    let mut stmts = vec![];
    println!("new scope");
    for stmt in block.stmts {
        let stmt_id = db.lower_stmt(stmt);
        stmts.push(stmt_id);
        // ...
    }
    println!("end scope");
    ir::Block(stmts)
}

fn lower_expr(db: &impl IRDatabase, expr: ast::Expr) -> ExprId {
    use ast::ExprKind;
    let expr = match expr.kind {
        ExprKind::Unary(op, expr) => {
            let id = db.lower_expr(*expr);
            ir::Expr::Unary(op, id)
            // ...
        }
        ExprKind::Binary(op, left, right) => {
            let lhs = db.lower_expr(*left);
            let rhs = db.lower_expr(*right);
            ir::Expr::Binary { lhs, rhs, op }
        }
        ExprKind::Lit(lit) => ir::Expr::Literal(match lit.kind {
            ast::LitKind::Bool(symbol) => ir::Literal::Bool(symbol.into()),
            ast::LitKind::Str(symbol) => ir::Literal::String(symbol.into()),
            ast::LitKind::Number(symbol) => ir::Literal::Number(symbol.into()),
        }),
        // Reference some variable
        ExprKind::Reference(ident) => {
            // We need to resolve which
            println!("referencing {:?}", ident);
            panic!("no");
        }
        _ => {
            todo!("");
        }
    };
    let id = db.intern_expr(expr.clone());
    println!("expr {:?} {:?}", expr, id);
    id
}
