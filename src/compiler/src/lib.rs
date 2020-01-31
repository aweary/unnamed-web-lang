#![warn(clippy::all)]
#![allow(dead_code)]

mod compiler;
mod ir;
mod lowering;
mod ctx;

pub use compiler::*;
