use compiler;
use env_logger;
use std::env;

fn main() {
    env_logger::init();
    let path = env::current_dir().unwrap().join("fixtures/files/main.dan");
    compiler::run_from_path(path);
}
