use crate::ctx::Context;

use parser::Parser;
// TODO import these from diagnostics
use diagnostics::{FileId, ParseResult};
use std::path::PathBuf;

use crate::lowering::lower_module;
use crate::passes::DCEPass;

use hir;

use syntax::ast::*;

// Root compiler instance.
struct Compiler {
    // ...
}

fn parse_module_from_path(ctx: &mut Context, path: &PathBuf) -> ParseResult<(Mod, FileId)> {
    let file_id = ctx.add_file(path).unwrap();
    let source = ctx.resolve_file(file_id);
    // TODO maybe parser should just get the file_id and ctx reference?
    let mut parser = Parser::new(source, file_id);
    Ok((parser.parse_module()?, file_id))
}

/// Run the compiler, pointing at a directory. We expect
/// to resolve {dir}/main.dom as the entrypoint.  
pub fn run_from_source_root(path: PathBuf) {
    // Resolve the entry point path, which we currently assume is main.dom
    let entry_point = path.join("main.dom");
    println!("run_from_source_root {:?}", entry_point);
    let mut ctx = Context::new();
    match lower_module(&mut ctx, &entry_point) {
        Ok(_) => {
            // Eliminate dead code
            // DCEPass::run(&mut ctx, module_id);
            // ...
        }
        Err(diagnostic) => {
            // ...
            ctx.emit_diagnostic(diagnostic);
        }
    };
}
