use compiler;

pub fn main() {
    let path_str = "fixture.dom";
    compiler::run_from_file(path_str);
}
