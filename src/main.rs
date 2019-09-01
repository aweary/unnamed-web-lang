mod ast;
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
        component Button(label: string) : HTMLButton {
           return <div class=\"foo\" isDisabled={label == 5}>{label}</div>
        }
        ",
    );
    match Parser::parse(&program) {
        Ok(module) => {
            println!("{:#?}", module);
        }
        Err(err) => {
            println!("{:?}", err);
        }
    };
}
