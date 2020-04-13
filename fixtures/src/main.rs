use compiler;
use std::env;

fn main() {
    let path = env::current_dir().unwrap().join("fixtures/files/main.dom");
    println!("path: {:?}", path);
    match compiler::run_from_path(path) {
        Ok(_) => println!("Compiled!"),
        Err(err) => {
            println!("Ooops! {:#?}", err);
        }
    }
}
