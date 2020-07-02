use log::info;
use std::env;
use std::time::Instant;

fn main() {
    env_logger::init();
    let start = Instant::now();
    let path = env::current_dir().unwrap().join("fixtures/files/main.dan");
    compiler::run_from_path(path);
    let end = Instant::now();
    info!(
        "Running fixture took: {:?}ms",
        end.duration_since(start).as_millis()
    );
}
