#![warn(clippy::all)]

mod compiler;
mod context;
mod ir;
mod passes;

pub use compiler::*;
