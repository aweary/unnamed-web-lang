/// Builds up the dependency graph for the module system.
use crate::context::Context;
use syntax::ast::*;
use syntax::visitor::Visitor;

/// The first type checking pass. Also responsible for name resolution.
/// Is able to check and cache most types. Polymorphic structs or functions
/// will be revalidated in a second pass.
pub struct DepGraphPass<'a> {
    pub ctx: &'a mut Context,
}

impl<'a> Visitor for DepGraphPass<'a> {
    fn visit_import(&mut self, _import: &mut Import) {
        println!("import!");
        // // Get the parent path of the current file, so we can resolve
        // // the import relative to that directory. This doesn't handle
        // // absolute file paths or named imports (like 3rd-party packages)
        // let current_file = self.ctx.sess.source_map.current_file.clone().unwrap();
        // let root_path = current_file.abs_path.parent().unwrap();
        // // Need to strip
        // let relative_path = import.path.path.clone();
        // let resolved = root_path.join(relative_path.clone());
        // match resolved.canonicalize() {
        //     Ok(path) => {
        //         println!("resolved {:?}", path);
        //         self.ctx.sess.source_map.load_file_from_path(&path);
        //     }
        //     Err(_) => {
        //         // TODO handle module resolution failure.
        //         println!("Failed to resolve!");
        //         // ...
        //     }
        // }
    }
}
