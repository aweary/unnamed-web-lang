use std::path::PathBuf;

use diagnostics::{FileId, ParseResult};
use lowering::LoweringContext;
use parser::Parser;
use session::Session;
use syntax::ast::*;

use codegen::codegen_module;

// Root compiler instance.
struct Compiler<'sess> {
    sess: &'sess mut Session,
    // ...
}

impl Compiler<'_> {
    pub fn parse_from_path(&mut self, path: &PathBuf) -> ParseResult<(Module, FileId)> {
        let file_id = self.sess.add_file(path).unwrap();
        let source = self.sess.resolve_file(file_id);
        // TODO maybe parser should just get the file_id and ctx reference?
        let mut parser = Parser::new(source, file_id);
        Ok((parser.parse_module()?, file_id))
    }
}

fn run_from_source_root_impl(path: PathBuf, sess: &mut Session) -> ParseResult<()> {
// Shared state for the entire parsing session
    let (module_ast, file_id) = Compiler { sess }
        .parse_from_path(
            // Resolve the entry point path, which we currently assume is main.dom
            &path.join("main.dom"),
        )?;
    let module = LoweringContext::new(sess, file_id).lower_module(module_ast)?;
    codegen_module(module)?;
    Ok(())
}

/// Run the compiler, pointing at a directory. We expect
/// to resolve {dir}/main.dom as the entrypoint.  
pub fn run_from_source_root(path: PathBuf) -> ParseResult<()> {
    let mut sess = Session::default();
    match run_from_source_root_impl(path, &mut sess) {
        Ok(_) => {
            // ...
        }
        Err(diagnostic) => {
            use codespan_reporting::term::emit;
            use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
            let writer = StandardStream::stderr(ColorChoice::Auto);
            let config = codespan_reporting::term::Config::default();
            emit(&mut writer.lock(), &config, &sess.files, &diagnostic).unwrap();
        }
    };
    Ok(())
    // match lower_module(&mut ctx, &entry_point) {
    //     Ok(_) => {
    //         // Eliminate dead code
    //         // DCEPass::run(&mut ctx, module_id);
    //         // ...
    //     }
    //     Err(diagnostic) => {
    //         // ...
    //         ctx.emit_diagnostic(diagnostic);
    //     }
    // };
}
