#![warn(clippy::all)]

pub mod infer;
pub mod symbol_table;
pub mod ty_context;

pub use infer::*;
pub use syntax::ty::*;
