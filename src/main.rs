mod ast;
mod context;
mod error;
mod lexer;
mod parser;
mod pos;
mod reader;
mod symbol;

// mod symbols;
mod token;
// use crate::error::ParseError;
use crate::parser::parse_module;
use std::fs;

fn main() {
    let filename = "main.dom";
    let program = fs::read_to_string(filename).expect("File not file");
    let results = parse_module(&program);
    println!("results {:?}", results);
}
