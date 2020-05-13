#![warn(
    clippy::all,
    clippy::pedantic,
)]
#![allow(clippy::must_use_candidate)]
#![allow(dead_code)]

mod compiler;

pub use compiler::*;
