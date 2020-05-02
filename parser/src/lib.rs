#![warn(clippy::all)]

mod parser;

pub mod test_utils;
pub use crate::parser::*;

#[cfg(test)]
mod tests {
    use crate::parser::Parser;
    use diagnostics::ParseResult as Result;
    use insta::assert_json_snapshot;
    use std::fs;
    use std::path::PathBuf;
    use syntax::{ast, precedence::Precedence};

    fn parse_stmts(source: &str) -> Vec<ast::Stmt> {
        let mut parser = Parser::new(source);
        parser.stmt_list().unwrap()
    }

    fn parse_module(source: &str) -> ast::Module {
        let mut parser = Parser::new(source);
        parser.parse_module().unwrap()
    }

    fn parse_expr(source: &str) -> ast::Expr {
        let mut parser = Parser::new(source);
        parser.expr(Precedence::NONE).unwrap()
    }

    fn expr_snapshot(name: &str, input: &str) {
        let name = format!("expr_{}", name);
        assert_json_snapshot!(name, parse_expr(input))
    }

    fn stmts_snapshot(name: &str, input: &str) {
        let name = format!("stmt_{}", name);
        assert_json_snapshot!(name, parse_stmts(input))
    }

    #[test]
    fn literal_snapshots() {
        expr_snapshot("integer", "1234");
        expr_snapshot("float", "12.34");
        expr_snapshot("long_float", "23.32342123423");
        expr_snapshot("string", "\"Hello\"");
        expr_snapshot("boolean_true", "true");
        expr_snapshot("boolean_false", "false");
        expr_snapshot("array", "[1, 2, 3]");
        expr_snapshot("object", "{a: 1, b: 2}");
    }

    #[test]
    fn let_stmt_snapshots() {
        stmts_snapshot(
            "numbers",
            "
          let a = 1
          let b = 2
          let c = 3.4
        ",
        );

        stmts_snapshot(
            "ident_names",
            "
        let a = 1
        let longerName = 2
        let longerNameWith_underscore = 3
        let _ = 4
        let _leadingUnderscore = 5
        ",
        )
    }

    #[test]
    fn fixtures() {
        let input = include_str!("./inputs/function_simple.dan");
        assert_json_snapshot!("function_simple_fixture", parse_module(input));
    }

}
