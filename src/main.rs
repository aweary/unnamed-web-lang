mod error;
mod lexer;
mod parser;
mod pos;
mod reader;
mod token;

use crate::parser::Parser;

fn main() {
    let program = String::from(
        "
        let foo = 5 + 2;
        let bar = foo / 5 + 2;
        ",
    );
    Parser::parse(&program);
}
