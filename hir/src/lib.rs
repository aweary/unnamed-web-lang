#![warn(
    clippy::all,
    clippy::pedantic,
)]
#![allow(clippy::must_use_candidate)]
#![allow(dead_code)]

pub mod unique_name;
pub mod hir;
pub mod visit;
pub use hir::*;
