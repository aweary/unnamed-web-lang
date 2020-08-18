#![warn(clippy::all, clippy::pedantic)]
#![allow(clippy::must_use_candidate)]

mod parser;

pub use crate::parser::*;

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use insta::assert_json_snapshot;
    use syntax::ast;

    fn parse_module(source: &str) -> ast::Module {
        let mut parser = Parser::new(source);
        parser.parse_module().unwrap()
    }

    #[test]
    fn fixture_function_simple() {
        let input = include_str!("./inputs/function_simple.dan");
        assert_json_snapshot!(parse_module(input));
    }

    #[test]
    fn fixture_control_flow() {
        let input = include_str!("./inputs/control_flow.dan");
        assert_json_snapshot!(parse_module(input));
    }

    #[test]
    fn fixture_trailing_closure_control_flow() {
        let input = include_str!("./inputs/trailing_closures.dan");
        assert_json_snapshot!(parse_module(input));
    }
}
