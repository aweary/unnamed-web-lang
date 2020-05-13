#![warn(
    clippy::all,
    clippy::pedantic,
)]
#![allow(clippy::must_use_candidate)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
// #[macro_use(quickcheck)]
// extern crate quickcheck_macros;
pub mod code_fuzz;

#[cfg(test)]
mod tests {}
