#![warn(clippy::all)]
#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
// #[macro_use(quickcheck)]
// extern crate quickcheck_macros;

pub mod code_fuzz;

#[cfg(test)]
mod tests {}
