mod ast;
mod error;
mod ir;
mod lexer;
mod parser;
mod passes;
mod pos;
mod reader;
mod result;
mod symbol;
mod token;
mod typecheck;
mod visitor;

use std::fs;

use crate::parser::{parse_program, ParsingContext};
use crate::passes::resolution::NameResolutionPass;
use crate::passes::templates::TemplateIRPass;

pub struct Compiler {}

impl Compiler {
    pub fn run(&mut self, path: &str) {
        let program = fs::read_to_string(path).expect("File not file");
        let mut ctx = ParsingContext::new();
        match parse_program(&program, &mut ctx) {
            Ok(ast) => {
                let mut resolve_names = NameResolutionPass::new();
                let mut template_ir = TemplateIRPass::new();
                for module in &ast.modules {
                    resolve_names.populated_symbol_table(&mut ctx, &module);
                    template_ir.compile_templates(&mut ctx, &module);
                }
            }
            Err(err) => {
                println!("error: {:?}", err);
            }
        }
    }
}

fn main() {
    let filename = "main.dom";
    let mut compiler = Compiler {};
    compiler.run(&filename);
}
