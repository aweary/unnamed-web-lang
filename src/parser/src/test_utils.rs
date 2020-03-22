// use crate::parser::Parser;
// use syntax::ast::{FnDef, Stmt, Block};
// use diagnostics::ParseResult;



// /// Parse a single statement
// pub fn parse_statement(input: &str) -> Stmt {
//     let mut files = Files::new();
//     let file_id = files.add("statement", input);
//     let mut parser = Parser::new(input, file_id);
//     parser.stmt().unwrap()
// }

// pub fn parse_fn_def(input: &str) -> FnDef {
//     let mut files = Files::new();
//     let file_id = files.add("fn_def", input);
//     let mut parser = Parser::new(input, file_id);
//     parser.fn_def().unwrap()
// }

// pub fn parse_block(input: &str) -> ParseResult<Block> {
//     let mut files = Files::new();
//     let file_id = files.add("block", input);
//     let mut parser = Parser::new(input, file_id);
//     parser.block()
// }
