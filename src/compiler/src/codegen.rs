// TODO this should move to the codegen crate once
// we're ready to reorganize the dependencies

use crate::lowering::IRDatabase;
use parser::ParserDatabase;
use salsa;

use crate::ir::*;
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
        self.add_comment("Compiled by WebScript");
        let mut module = self.db.lower_module(module_id);
        for def_id in module.items {
            let def = self.db.lookup_intern_def(def_id);
            self.codegen_def(def);
        }
        self.code
    }

    fn codegen_def(&mut self, def: Def) {
        match def.kind {
            DefKind::Func { name, params, body } => {
                self.push("function ");
                self.push(name.to_str());
                self.push("(");
                for (i, param) in params.into_iter().enumerate() {
                    if i > 0 {
                        self.push(",");
                    }
                    self.push(param.name().as_str());
                }
                self.push(")");
                self.codegen_fn_body(body);
                // ...
            }
            _ => {
                todo!();
            }
        };
    }

    fn codegen_fn_body(&mut self, body: Block) {
        self.push("{\n");
        for stmt_id in body.0 {
            self.codegen_stmt(stmt_id);
        }
        self.push("}\n");
    }

    fn codegen_stmt(&mut self, id: StmtId) {
        let stmt = self.db.lookup_intern_stmt(id);
        match stmt.kind {
            StmtKind::Return(expr_id) => {
                self.push("return ");
                self.codegen_expr(expr_id);
            }
            StmtKind::Local(local_id) => {
                let local = self.db.lookup_intern_local(local_id);
                let name = local.name.as_str();
                self.push("let ");
                // TODO renaming, minification
                self.push(name);
                if let Some(expr_id) = local.init {
                    self.push(" = ");
                    self.codegen_expr(expr_id);
                }
                // self.push(local);
            }
            _ => {
                // todo!();
            }
        };
        self.push(";\n");
    }

    fn codegen_expr(&mut self, id: ExprId) {
        let expr = self.db.lookup_intern_expr(id);
        match expr.kind {
            ExprKind::Literal(lit) => {
                self.push(&format!("{}", lit));
            }
            ExprKind::Reference(reference) => {
                match reference {
                    Reference::Local(local_id) => {
                        // TODO check if we should inline this?
                        let local = self.db.lookup_intern_local(local_id);
                        let name : syntax::symbol::Symbol = local.name.into();
                        self.push(name.as_str());
                    }
                    _ => (),
                };
            }
            _ => (),
        };
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
            self.push(param.name().as_str());
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
