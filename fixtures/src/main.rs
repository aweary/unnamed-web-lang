use compiler;
use std::env;

fn main() {
    let path = env::current_dir().unwrap().join("fixtures/files/main.dan");
    println!("path: {:?}", path);
    compiler::run_from_path(path); 
}
