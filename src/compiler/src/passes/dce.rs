use super::Visitor;
use crate::ctx::Context;
use crate::ir::*;

pub struct DCEPass<'ctx> {
    ctx: &'ctx mut Context,
}

impl<'ctx> DCEPass<'ctx> {
    pub fn run(ctx: &'ctx mut Context, module_id: ModuleId) {
        let mut dce_pass = DCEPass::new(ctx);
        dce_pass.run_on_module(module_id);
    }

    fn new(ctx: &'ctx mut Context) -> Self {
        DCEPass { ctx }
    }

    fn run_on_module(&mut self, module_id: ModuleId) {
        match self.ctx.module_arena.get_mut(module_id) {
            Some(module) => {
                // First we check if top-level items are ever used or exported
                // for item_id in &module.items {
                //     let refs = self.ctx.reference_map.resolve_item_references(*item_id);
                //     println!("REFERENCES {:?}", refs);
                //     // ...
                // }
                // ...
            }
            None => {
                panic!("Fatal error, cannot resolve module");
            }
        }
    }
}
