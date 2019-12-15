use crate::context::Context;
use syntax::ast::*;
use syntax::symbol::Symbol;
use syntax::visitor::{
    walk_block, walk_expr, walk_fn_def, walk_item, walk_local_pattern, walk_stmt, Visitor,
};

use typecheck::infer::infer_fn;

/// The first type checking pass. Also responsible for name resolution.
/// Is able to check and cache most types. Polymorphic structs or functions
/// will be revalidated in a second pass.
pub struct EarlyTypecheckPass<'a> {
    pub ctx: &'a mut Context,
    /// Whether a new block requires a new scope entry. Always true
    /// except for function defintions, as the scope is defined
    /// earlier to include the parameters.
    block_needs_scope: bool,
    param_ty: Option<Ty>,
}

impl<'a> EarlyTypecheckPass<'a> {
    pub fn new(ctx: &'a mut Context) -> EarlyTypecheckPass {
        EarlyTypecheckPass {
            ctx,
            block_needs_scope: true,
            param_ty: None,
        }
    }
}

// Resolve a single Symbol from a pattern.
// This will need to be extended to support resolving *multiple*
// symbols, so we can typecheck destructuring.
fn resolve_symbol_from_pattern(pattern: &LocalPattern) -> Symbol {
    match pattern {
        LocalPattern::Ident(ident, _) => {
            // TODO don't clone, should be Copy implicitly when interned
            ident.name.clone()
        }
        _ => unimplemented!("Cant type check destructure yet"),
    }
}

impl<'a> Visitor for EarlyTypecheckPass<'a> {
    fn visit_block(&mut self, block: &mut Block) {
        if self.block_needs_scope {
            self.ctx.push_scope();
        } else {
            // Reset the flag here.
            self.block_needs_scope = true;
        }
        walk_block(self, block);
        self.ctx.pop_scope();
    }

    fn visit_ident(&mut self, ident: &mut Ident) {
        // If param_ty exists, we are parsing a function parameter.
        if let Some(ty) = &self.param_ty {
            self.ctx.define_local(ident, ty);
            // ...
        }
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        if let StmtKind::Expr(expr) = &mut stmt.kind {
            let ty = self.ctx.infer(expr);
            if ty != Ty::Unit {
                // Warn about an expression statement evaluating
                // to something other than unit.
            }
        };
        walk_stmt(self, stmt);
    }

    fn visit_item(&mut self, item: &mut Item) {
        let name = &item.ident.name;
        // TODO this is translating a function AST node to the type. Probably not the best place for it.
        // It also doesn't accurately translate the types (e.g, for multiple arguments or polymorphic functions).
        match &item.kind {
            ItemKind::Fn(fn_def) => {
                // TODO this should be happening via self.ctx, infer_fn shouldnt be public
                let ty = infer_fn(&mut self.ctx.ty_context, fn_def).unwrap();
                self.ctx.ty_context.define(name.clone(), ty);
            }
            ItemKind::Type(ref _type_def) => {
                // println!("{:#?}", type_def);
            }
            _ => {
                // println!("Unrecognized item");
            }
        };
        walk_item(self, item);
    }

    fn visit_fn_def(&mut self, def: &mut FnDef) {
        self.ctx.push_scope();
        self.block_needs_scope = false;
        walk_fn_def(self, def);
    }

    fn visit_param(&mut self, param: &mut Param) {
        // Resolve the type annotation, TODO this is bad
        self.param_ty = Some(param.ty.clone());
        walk_local_pattern(self, &mut param.local);
        self.param_ty = None;
    }

    fn visit_reference(&mut self, _ident: &mut Ident) {
        // if !self.ctx.symbols.resolve(&ident.name) {
        //     // TODO should be fatal, how to handle errors in passes?
        //     self.ctx
        //         .sess
        //         .emit_warning("Reference error", "Uknown variable", ident.span)
        // }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        // TODO how to handle type checking all expressions? Not just assignments.
        walk_expr(self, expr);
    }

    fn visit_local(&mut self, local: &mut Local) {
        walk_local_pattern(self, &mut local.name);

        // Get the symbol for this local.
        let symbol = resolve_symbol_from_pattern(&local.name);
        // TODO shouldnt be Option
        if let Some(init) = &mut local.init {
            walk_expr(self, init);
            let ty = self.ctx.infer(init);
            println!("{:?}", ty);
            self.ctx.ty_context.define(symbol, ty);
        }
    }
}
