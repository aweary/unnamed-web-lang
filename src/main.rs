mod ast;
mod error;
mod lexer;
mod parser;
mod pos;
mod reader;
mod token;

use crate::parser::Parser;
use std::fs;

fn main() {
    let filename = "main.dom";
    let program = fs::read_to_string(filename).expect("File not file");
    match Parser::parse(&program) {
        Ok(module) => {
            println!("{:?}", module);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    };
}
