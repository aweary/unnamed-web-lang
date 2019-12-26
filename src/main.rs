use compiler;
use std::env;

pub fn main() {
    // Pretend this path is coming from the CLI
    let path = env::current_dir().unwrap().join("fixture");
    compiler::run_from_source_root(path);
}
