#![warn(clippy::all)]

pub mod infer;
pub mod ty_context;
pub mod symbol_table;

pub use infer::*;
pub use syntax::ty::*;
