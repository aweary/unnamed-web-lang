#![warn(
    clippy::all,
    clippy::pedantic,
)]
#![allow(clippy::must_use_candidate)]

mod lexer;
mod reader;
mod result;

pub use lexer::*;
