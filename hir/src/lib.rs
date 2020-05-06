#![warn(clippy::all)]
#![allow(dead_code)]

pub mod unique_name;
pub mod hir;
pub mod visit;
pub use hir::*;
