#![warn(clippy::all)]

mod codegen;
mod compiler;
mod ir;
mod lowering;

pub use compiler::*;
