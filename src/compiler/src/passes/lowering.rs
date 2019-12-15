/// Lower to IR
use crate::context::Context;
use syntax::ast::*;
// use syntax::symbol::Symbol;
use syntax::visitor::{
    walk_block, walk_expr, walk_fn_def, walk_item, walk_local_pattern, walk_stmt, Visitor,
};

/// The first type checking pass. Also responsible for name resolution.
/// Is able to check and cache most types. Polymorphic structs or functions
/// will be revalidated in a second pass.
pub struct LoweringPass<'a> {
    pub ctx: &'a mut Context,
}

impl<'a> LoweringPass<'a> {
    pub fn new(ctx: &'a mut Context) -> LoweringPass {
        LoweringPass { ctx }
    }
}

impl<'a> Visitor for LoweringPass<'a> {
    fn visit_block(&mut self, block: &mut Block) {
        // println!("Hit block, create a CFG?");
        walk_block(self, block);
    }

    fn visit_template(&mut self, template: &mut Template) {
        println!("Lowering a template to IR...");
    }
}
