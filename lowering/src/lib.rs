#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
)]
#![allow(clippy::must_use_candidate)]
#![allow(dead_code)]

pub mod lower;
pub use lower::*;
