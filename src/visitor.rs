use crate::ast::*;
use crate::symbol::Symbol;
use crate::parser::ParsingContext;

// Copied from libsytnax/visit.rs
#[macro_export]
macro_rules! walk_list {
    ($visitor: expr, $ctx: ident, $method: ident, $list: expr) => {
        for elem in $list {
            $visitor.$method($ctx, elem)
        }
    };
    // ($visitor: expr, $method: ident, $list: expr, $($extra_args: expr),*) => {
    //     for elem in $list {
    //         $visitor.$method(elem, $($extra_args,)*)
    //     }
    // }
}

pub trait Visitor<'ast>: Sized {

    fn visit_mod(&mut self, ctx: &ParsingContext, module: &'ast Module) {
        walk_mod(self, &ctx, &module);
    }

    fn visit_ident(&mut self, _sym: &Symbol) {
        // ...
    }

    fn visit_let(&mut self, ctx: &ParsingContext, decl: &'ast LetDecl) {
        // ...
    }
    

    fn visit_expr(&mut self, ctx: &ParsingContext, expr: ExprId) {
        walk_expr(self, &ctx, expr);
    }

    fn visit_call_expr(&mut self, call: &'ast Call) {
      // ...
    }

    fn visit_block(&mut self, ctx: &ParsingContext, block: &'ast Block) {
        walk_block(self, &ctx, &block);
    }

    fn visit_stmt(&mut self, ctx: &ParsingContext, stmt: &'ast Stmt) {
        walk_stmt(self, &ctx, &stmt);
    }

    fn visit_fn(&mut self, ctx: &ParsingContext, func: &'ast FuncDecl) {
        walk_fn(self, &ctx, &func)
    }

    fn visit_decl(&mut self, ctx: &ParsingContext, decl: &'ast Decl) {
        use DeclKind::*;
        match decl.kind {
            Func(ref decl) => {
                self.visit_fn(&ctx, &decl);
            }
            _ => {}
        };
    }
}

pub fn walk_mod<'ast, V: Visitor<'ast>>(
    visitor: &mut V,
    ctx: &ParsingContext,
    module: &'ast Module,
    ) {
    walk_list!(visitor, ctx, visit_decl, &module.stmts);
}

pub fn walk_stmt<'ast, V: Visitor<'ast>>(visitor: &mut V, ctx: &ParsingContext, stmt: &'ast Stmt) {
    use StmtKind::*;
    match *stmt.kind {
        LetDecl(ref decl) => visitor.visit_let(&ctx, decl),
        Return(expr) => visitor.visit_expr(&ctx, expr),
        If(expr, ref block, ref _alt) => {
            visitor.visit_expr(&ctx, expr);
            visitor.visit_block(&ctx, &block);
        }
        _ => {
            // ...
        }
    }
}

pub fn walk_fn<'ast, V: Visitor<'ast>>(visitor: &mut V, ctx: &ParsingContext, func: &'ast FuncDecl) {
    visitor.visit_block(&ctx, &func.block);
}

pub fn walk_expr<'ast, V: Visitor<'ast>>(visitor: &mut V, ctx: &ParsingContext, expr_id: ExprId) {
    let expr = ctx.resolve_expr(expr_id);
    match *expr.kind {
        ExprKind::Ident(sym) => {
            visitor.visit_ident(&sym);
        }
        ExprKind::Binary {
            left,
            right,
            ..
        } => {
            visitor.visit_expr(&ctx, left);
            visitor.visit_expr(&ctx, right);
        }
        ExprKind::Call(ref call) => {
          visitor.visit_expr(&ctx, call.callee);
          for expr in &call.arguments {
            visitor.visit_expr(&ctx, *expr);
          }
          // visitor.visit_call_expr(&call);
          // ...
        }
        _ => {}
    }
}

pub fn walk_block<'ast, V: Visitor<'ast>>(visitor: &mut V, ctx: &ParsingContext, block: &'ast Block) {
    walk_list!(visitor, ctx, visit_stmt, &block.0);
}
