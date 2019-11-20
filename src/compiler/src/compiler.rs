use parser::Parser;
use syntax::visitor::Visitor;

use codespan::FileId;
// TODO import these from diagnostics
use codespan_reporting::term::emit;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};

// Compiler passes
use crate::context::Context;
use crate::passes::EarlyTypecheckPass;

#[derive(Default)]
pub struct Compiler {
    ctx: Context,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            ctx: Context::new(),
        }
    }
}

impl Compiler {
    fn parse(&mut self, file_id: FileId) {
        let source = self.ctx.sess.source_map.files.source(file_id);
        let mut parser = Parser::new_from_str(source, &self.ctx.sess);
        match parser.parse_program() {
            Ok(mut program) => {
                let mut name_resolution_pass = EarlyTypecheckPass::new(&mut self.ctx);
                name_resolution_pass.visit_program(&mut program);
            }
            Err(err) => {
                let writer = StandardStream::stderr(ColorChoice::Auto);
                let config = codespan_reporting::term::Config::default();
                emit(
                    &mut writer.lock(),
                    &config,
                    &self.ctx.sess.source_map.files,
                    &err,
                )
                .unwrap();
                // ...
            }
        }
    }

    pub fn run_from_file(&mut self, path: &str) {
        let id = self.ctx.sess.source_map.load_file(path);
        self.parse(id);
    }
}

pub fn run_from_file(path: &str) {
    let mut compiler = Compiler::new();
    compiler.run_from_file(path);
}
