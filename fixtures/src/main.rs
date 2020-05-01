use compiler;
use std::env;
use env_logger;

fn main() {
    env_logger::init();
    let path = env::current_dir().unwrap().join("fixtures/files/main.dan");
    compiler::run_from_path(path); 
}
