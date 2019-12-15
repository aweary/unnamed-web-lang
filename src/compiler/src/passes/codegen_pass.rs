/// Lower to IR
use crate::context::Context;
use syntax::ast::*;

use codegen::codegen_fn;
// use syntax::symbol::Symbol;
use syntax::visitor::{
    walk_block, walk_expr, walk_fn_def, walk_item, walk_local_pattern, walk_stmt, Visitor,
};

/// The first type checking pass. Also responsible for name resolution.
/// Is able to check and cache most types. Polymorphic structs or functions
/// will be revalidated in a second pass.
pub struct CodegenPass<'a> {
    pub ctx: &'a mut Context,
}

impl<'a> CodegenPass<'a> {
    pub fn new(ctx: &'a mut Context) -> CodegenPass {
        CodegenPass { ctx }
    }
}

impl<'a> Visitor for CodegenPass<'a> {
    fn visit_fn_def(&mut self, def: &mut FnDef) {
        codegen_fn(def);
    }
}
