use crate::parser::Parser;

use salsa;
use source::{ModuleId, Source};
use syntax::ast;

use diagnostics::Diagnostic;

use std::path::PathBuf;

pub trait ErrorReporting {
    fn report_diagnostic(&self, diagnostic: Diagnostic);
}

#[salsa::query_group(ParserDatabaseStorage)]
pub trait ParserDatabase: Source + ErrorReporting {
    #[salsa::interned]
    fn intern_module_path(&self, path: PathBuf) -> ModuleId;
    // #[salsa::cycle(module_dep_cycle)]
    // fn module_deps(&self, module_id: ModuleId) -> Vec<ModuleId>;
    fn parse_module(&self, module: ModuleId) -> ast::Mod;
}

fn parse_module(db: &impl ParserDatabase, id: ModuleId) -> ast::Mod {
    // Get the path for the module ID
    let path = db.lookup_intern_module_path(id);
    // Find the associated file ID. This is important for error reporting.
    let file_id = db.resolve_file(path);
    // Read the source text for the file.
    let source = db.source_text(file_id);
    match Parser::new(&source, file_id, db).parse_module() {
        Ok(res) => res,
        Err(err) => {
            db.report_diagnostic(err);
            // TODO we shouldn't panic, we can recover. Maybe we can have a special
            // AST struct like ast::Failure and then map the ModuleId to a set of
            // errors that we can report later?
            todo!();
        }
    }
}
