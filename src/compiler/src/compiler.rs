use parser::{ErrorReporting, ParserDatabase, ParserDatabaseStorage};
// TODO import these from diagnostics
use crate::codegen::{CodegenDatabase, CodegenDatabaseStorage};
use crate::ir::SpanId;
use crate::lowering::{IRDatabase, IRDatabaseStorage, LoweringT, LoweringCtxt};
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use diagnostics::Diagnostic;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

use std::cell::Cell;
use codespan::Files;
use log::{self, debug};
use salsa;
use source::*;


/// Stores all data necsesary for parsing
// struct CompilerRoot {

// }

#[salsa::database(
    SourceDatabase,
    ParserDatabaseStorage,
    IRDatabaseStorage,
    CodegenDatabaseStorage
)]

#[derive(Default)]
pub struct CompilerDatabase {
    runtime: salsa::Runtime<CompilerDatabase>,
    current_module_id: Cell<Option<ModuleId>>,
}

impl LoweringCtxt for CompilerDatabase {
    fn set_current_module_id(&self, module_id: ModuleId) {
        self.current_module_id.set(Some(module_id))
    }

    fn get_current_module_id(&self) -> ModuleId {
        self.current_module_id.get().unwrap()
    }
}

// TODO this should be moved out somewhere...
impl LoweringT for CompilerDatabase {
    fn map_id_to_span(&self, id: impl salsa::InternKey, span_id: SpanId) {
        // ...
    }

    fn define(&self, ident: syntax::ast::Ident, id: crate::ir::LocalId) {
        println!("Mapping {:?} to {:?}", ident, id);
        // ...
    }
}

/// Report errors gracefully
/// TODO ErrorReporting should be part of the diagnostics crate
impl ErrorReporting for CompilerDatabase {
    fn report_diagnostic(&self, diagnostic: Diagnostic) {
        let files_db = self.files(());
        let files = files_db.lock().unwrap();
        let writer = StandardStream::stderr(ColorChoice::Auto);
        let config = codespan_reporting::term::Config::default();
        emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}

impl salsa::Database for CompilerDatabase {
    fn salsa_runtime(&self) -> &salsa::Runtime<Self> {
        &self.runtime
    }

    fn salsa_runtime_mut(&mut self) -> &mut salsa::Runtime<Self> {
        &mut self.runtime
    }
}

impl CompilerDatabase {
    pub fn new() -> Self {
        // TODO move log::init to a more appropriate place
        log::init();
        let mut db = CompilerDatabase::default();
        db.set_files((), Arc::new(Mutex::new(Files::new())));
        db
    }
}

/// Run the compiler, pointing at a directory. We expect
/// to resolve {dir}/main.dom as the entrypoint.  
pub fn run_from_source_root(path: PathBuf) {
    let entry_point = path.join("main.dom");
    debug!("run_from_source_root {:?}", entry_point);
    let db = CompilerDatabase::new();
    let module_id = db.intern_module_path(entry_point);
    db.set_current_module_id(module_id);
    let code = db.codegen_module(module_id);
    println!("{}", code);
}
