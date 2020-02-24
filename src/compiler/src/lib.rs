#![warn(clippy::all)]
#![allow(dead_code)]

mod compiler;
mod ctx;
mod ir;
mod lowering;
mod passes;
mod scope;
mod visit;

pub use compiler::*;
