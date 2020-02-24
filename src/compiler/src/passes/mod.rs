///
mod dce;

pub use dce::*;

use crate::ctx::Context;
use crate::ir::*;

pub trait Visitor {
    fn visit_module(&mut self, ctx: &mut Context, module_id: ModuleId) {
        // ...
    }

    fn visit_item(&mut self, ctx: &mut Context, item_id: ItemId) {
        // ...
    }
}

fn walk_module<T: Visitor>(visitor: &mut T, ctx: &mut Context, module_id: ModuleId) {
    // let module =  ctx.module_arena.get_mut(module_id).unwrap();
    // for item_id in &mut module.items {
    //     visitor.visit_item(ctx, *item_id);
    // }
}
