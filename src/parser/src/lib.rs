#![warn(clippy::all)]

mod parser;

pub mod test_utils;
pub use crate::parser::*;

#[cfg(test)]
mod tests {
    // use crate::test_utils::parse_statement;
    // use insta::assert_debug_snapshot;

    // fn assert_stmt_snapshot(input: &str) {
    //     assert_debug_snapshot!(parse_statement(input));
    // }

    // #[test]
    // fn expr_stmt() {
    //     assert_stmt_snapshot("500;");
    //     assert_stmt_snapshot("40.02;");
    //     assert_stmt_snapshot("\"Hello\";");
    //     assert_stmt_snapshot("[1, 2, \"3\"];");
    // }
}
