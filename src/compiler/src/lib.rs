#![warn(clippy::all)]
#![allow(dead_code)]

mod compiler;
mod ctx;
mod lowering;
mod visit;

pub use compiler::*;
