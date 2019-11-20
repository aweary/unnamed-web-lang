use syntax::ast::{self, *};
use syntax::sess::ParseSess;

use typecheck::{LiteralTy, Ty};
use typecheck::infer;
use typecheck::ty_context::TyContext;

/// The shared parsing context for a Compiler instance.
/// Provides an API for name and type resolution, among other things.
#[derive(Default)]
pub struct Context {
    // TODO this shouldnt be public, add proxy methods
    pub sess: ParseSess,
    pub ty_context: TyContext
}

impl Context {
    pub fn new() -> Context {
        Context {
            sess: Default::default(),
            ty_context: Default::default(),
        }
    }

    pub fn push_scope(&mut self) {
        self.ty_context.push_scope();
    }

    pub fn pop_scope(&mut self) {
        self.ty_context.pop_scope();
    }

    pub fn define_local(&mut self, local: &mut Ident, _ty: &Ty) {
        // self.symbols.define_local(local);
    }

    pub fn infer(&mut self, expr: &Expr) -> Ty {
        match infer(&mut self.ty_context, expr) {
            Ok(ty) => ty,
            Err((name, span)) => {
                self.sess.emit_error("Type Error", &name, span);
                panic!();
            }
        }
    }
}
