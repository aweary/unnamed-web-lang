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

pub struct Compiler {}

impl Compiler {
    pub fn run(&mut self, path: &str) {
        let program = fs::read_to_string(path).expect("File not file");
        let mut ctx = ParsingContext::new();
        let ast = parse_program(&program, &mut ctx).unwrap();
        let mut resolve_names = NameResolutionPass::new();
        for module in &ast.modules {
            resolve_names.populated_symbol_table(&mut ctx, &module);
        }
    }
}

fn main() {
    let filename = "main.dom";
    let mut compiler = Compiler {};
    compiler.run(&filename);
}
