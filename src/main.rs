mod lexer;
mod char;
mod token;
mod str;

use std::fs::File;
use std::io;
use std::io::prelude::*;
use std::io::BufReader;
use lexer::Lexer;

fn read_file(filename: String) -> io::Result<String> {
    let f = File::open(filename)?;
    let mut reader = BufReader::new(f);
    let mut buffer = String::new();
    reader.read_to_string(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let filename = String::from("main.dom");
    let buffer = read_file(filename).unwrap();
    let lexer = Lexer::new(buffer);
    for token in lexer {
        println!("{:?}", token);
    }
}
