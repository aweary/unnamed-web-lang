#[cfg(test)]
mod tests {
    use parser::Parser;
    use std::fs;
    use insta::{glob, assert_json_snapshot};
    #[test]
    // Easy but very noisy testing. Hard to debug when something fails.
    // Good for now, but more granular tests in the future will be great.
    fn parser_tests() {
        glob!("fixtures/*.dan", |path| {
            let input = fs::read_to_string(path).unwrap();
            let mut parser = Parser::new(&input);
            let ast = parser.parse_module().unwrap();
            assert_json_snapshot!(ast);
        });
    }
}