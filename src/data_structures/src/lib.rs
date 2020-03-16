#![warn(clippy::all)]
#![allow(dead_code)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod control_flow_graph;
pub mod arena;
pub mod scope_map;

pub use control_flow_graph::*;
pub use arena::*;

pub use fxhash::FxHashSet as HashSet;
pub use fxhash::FxHashMap as HashMap;
