use std::env;
use std::path::PathBuf;

pub fn resolve_path(path_str: &str) -> PathBuf {
    PathBuf::from(path_str)
}

pub fn absolute_path(path_str: &str) -> PathBuf {
    let path = resolve_path(path_str);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        let cwd = env::current_dir().unwrap();
        cwd.join(path)
    }
}
