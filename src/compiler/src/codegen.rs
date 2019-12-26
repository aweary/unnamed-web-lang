// TODO this should move to the codegen crate once
// we're ready to reorganize the dependencies

use crate::lowering::IRDatabase;
use parser::ParserDatabase;
use salsa;

use source::ModuleId;
use syntax::{ast, visitor::Visitor};
// use crate::ir::{self, DefId};

/// The codegen builder is responsible for tracking all of the
/// context needed to effectively codegen and giving easy access
/// for data stored in the Salsa database.
///
/// We need to track things like appropriate local names, imports,
/// and more.
struct CodegenBuilder<'a, T> {
    db: &'a T,
    code: String,
    // ...
}

impl<'a, T> CodegenBuilder<'a, T>
where
    T: CodegenDatabase,
{
    pub fn new(db: &'a T) -> Self {
        CodegenBuilder {
            db,
            code: String::new(),
        }
    }

    fn push(&mut self, text: &str) {
        self.code.push_str(text)
    }

    fn add_comment(&mut self, text: &str) {
        // These comments always include a newline for now
        self.push(&format!("/* {} */\n", text));
    }

    pub fn codegen_module(mut self, module_id: ModuleId) -> String {
        let mut module = self.db.lower_module(module_id);
        for def_id in module.items {
            let def = self.db.lookup_intern_def(def_id);
            println!("item! {:?}", def);
        }
        self.add_comment("Compiled by WebScript");
        // self.visit_mod(&mut module);
        self.code
    }
    // ...
}

impl<'a, T> Visitor for CodegenBuilder<'a, T>
where
    T: CodegenDatabase,
{
    /// Codegen a function definition
    fn visit_fn_def(&mut self, def: &mut ast::FnDef) {
        let name = def.name.to_str();
        self.push(&format!("function {}(", name));
        for param in def.params.clone().into_iter() {
            self.push(param.name());
            self.push(",");
        }
        self.push(") { ... }\n");
    }
}

#[salsa::query_group(CodegenDatabaseStorage)]
pub trait CodegenDatabase: IRDatabase {
    // We don't currently support codegen'ing a single definition.
    // fn codegen_def(&self, id: DefId) -> String;
    fn codegen_module(&self, id: ModuleId) -> String;
}

fn codegen_module(db: &impl CodegenDatabase, id: ModuleId) -> String {
    let builder = CodegenBuilder::new(db);
    builder.codegen_module(id)
    // let mut builder = String::from("\n\n/* Compiled by WebScript */\n");
    // let module = db.parse_module(id);
    // // for def_id in module.items {
    // //     let def_code = db.codegen_def(def_id);
    // //     builder.push_str(&def_code);
    // // }
    // builder.push_str("\n\n");
    // builder
}
