#![warn(clippy::all)]

mod codegen;
mod compiler;
mod ir;
mod lowering;
mod intravisit;

pub use compiler::*;
