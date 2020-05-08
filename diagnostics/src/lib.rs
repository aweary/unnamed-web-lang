#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::cargo,
)]
#![allow(clippy::must_use_candidate)]

pub mod diagnostics;
pub mod error;
pub use diagnostics::*;
