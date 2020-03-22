#![warn(clippy::all)]
#![allow(dead_code)]

pub mod hir;
pub mod visit;
pub use hir::*;
mod scope;

