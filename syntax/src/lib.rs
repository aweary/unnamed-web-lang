#![warn(
    clippy::all,
    clippy::pedantic,
)]
#![allow(clippy::must_use_candidate)]

pub mod ast;
pub mod precedence;
pub mod symbol;
pub mod token;
pub mod visitor;
