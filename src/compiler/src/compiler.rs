use parser::Parser;
use syntax::visitor::Visitor;

use syntax::ast;
use syntax::sess::ParseSess;

use codespan::{FileId, Files};
// TODO import these from diagnostics
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

use crate::context::Context;
use crate::passes::DepGraphPass;
// Compiler passes
use std::path::PathBuf;

use std::sync::{Arc, Mutex};

// Parsing joined paths
use path_dedot::*;

use petgraph::Graph;

use salsa;
use salsa::{InternId, InternKey};

use log::{self, debug};
use source::files;

use source::*;

// TODO
use std::env;

// We generate module IDs from paths. They can be the keys for salsa::intern?
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DefId(pub u32);

// IR, move to other file...
mod ir {
    use super::DefId;
    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Module {
        pub items: Vec<DefId>,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub enum DefKind {
        Func,
        Type,
        Enum,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Hash)]
    pub struct Def {
        pub kind: DefKind
    }
}

// We generate module IDs from paths. They can be the keys for salsa::intern?
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

impl InternKey for ModuleId {
    fn from_intern_id(v: InternId) -> Self {
        ModuleId(v.as_u32())
    }

    fn as_intern_id(&self) -> InternId {
        InternId::from(self.0)
    }
}

impl InternKey for DefId {
    fn from_intern_id(v: InternId) -> Self {
        DefId(v.as_u32())
    }

    fn as_intern_id(&self) -> InternId {
        InternId::from(self.0)
    }
}

/// Root database

#[salsa::query_group(ParserDatabaseStorage)]
pub trait ParserDatabase: Source {
    #[salsa::interned]
    fn intern_module_path(&self, path: PathBuf) -> ModuleId;
    #[salsa::interned]
    fn intern_item(&self, item: ast::Item) -> DefId;
    #[salsa::interned]
    fn intern_def(&self, def: ir::Def) -> DefId;
    fn lower_item(&self, item: ast::Item) -> DefId;
    fn codegen_def(&self, def: DefId) -> String;
    // #[salsa::cycle(module_dep_cycle)]
    // fn module_deps(&self, module_id: ModuleId) -> Vec<ModuleId>;
    // Lower to IR
    fn lower_module(&self, module_id: ModuleId) -> ir::Module;
    fn parse_module(&self, module: ModuleId) -> ast::Mod;
}

fn codegen_def(db: &impl ParserDatabase, id: DefId) -> String {
    let item = db.lookup_intern_item(id);
    let mut builder = String::new();
    use ast::{ItemKind, FnDef};
    match item.kind {
        ItemKind::Fn(fn_def) => {
            let FnDef { name, .. } = fn_def;
            builder.push_str("function ");
            // TODO Into<&str> for Ident
            builder.push_str(name.name.as_str());
            builder.push_str("() { ... }");
            // ..
        }
        ItemKind::Enum(_, _) => {
            // ...
        }
        ItemKind::Type(_) => {
            // ...
        }
        _ => {
            // ...
        }
    }
    builder
}

fn lower_module(db: &impl ParserDatabase, id: ModuleId) -> ir::Module {
    println!("Lowering {:?}", id);
    let ast = db.parse_module(id);
    let mut items = vec![];
    for item in ast.items {
        let id = db.intern_item(item);
        let codegen = db.codegen_def(id);
        println!("codegen for {:?}: {:?}", id, codegen);
    }
    ir::Module { items }
}

fn lower_item(db: &impl ParserDatabase, item: ast::Item) -> DefId {
    use ast::ItemKind;
    let kind = match item.kind {
        ItemKind::Fn(_) => ir::DefKind::Func,
        ItemKind::Enum(_, _) => ir::DefKind::Enum,
        ItemKind::Type(_) => ir::DefKind::Type,
        _ => unreachable!("Unknown type")
    };
    db.intern_def(ir::Def {
        kind
    })
}

fn parse_module(db: &impl ParserDatabase, id: ModuleId) -> ast::Mod {
    let path = db.lookup_intern_module_path(id);
    println!("Parsing module {:?}", path);
    let file_id = db.resolve_file(path);
    let source = db.source_text(file_id);
    let sess = ParseSess::default();
    let mut parser = Parser::new_from_str(&source, &sess);
    match parser.parse_module() {
        Ok(res) => {
             res
        }
        Err(err) => {
            println!("err {:?}", err);
            panic!();
        }
    }
}

// fn module_deps(db: &impl ParserDatabase, module_id: ModuleId) -> Vec<ModuleId> {
//     let mut pass = ModuleCount::new(db);
//     pass.build_dep_graph(module_id)
// }

/// Called if salsa finds a cycle when resolving module dependencies
// fn module_dep_cycle(
//     _db: &impl ParserDatabase,
//     cycle: &[String],
//     _path: &ModuleId,
// ) -> Vec<ModuleId> {
//     eprintln!("MODULE CYCLE DETECTED");
//     eprintln!("{:?}", cycle);
//     vec![]
// }

#[salsa::database(SourceDatabase, ParserDatabaseStorage)]
#[derive(Default)]
pub struct CompilerDatabase {
    runtime: salsa::Runtime<CompilerDatabase>,
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
    let hir = db.lower_module(module_id);
    println!("hir {:#?}", hir);
}
