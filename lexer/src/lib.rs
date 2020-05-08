#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
)]
#![allow(clippy::must_use_candidate)]

mod lexer;
mod reader;
mod result;

pub use lexer::*;
