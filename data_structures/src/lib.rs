#![warn(clippy::all)]
#![allow(dead_code)]

#[cfg(test)]
extern crate quickcheck;
#[cfg(test)]
// #[macro_use(quickcheck)]
extern crate quickcheck_macros;

pub mod arena;
pub mod control_flow_graph;
pub mod module_graph;
pub mod scope_map;

pub use arena::*;
pub use control_flow_graph::*;

pub use fxhash::FxHashMap as HashMap;
pub use fxhash::FxHashSet as HashSet;

pub use petgraph::*;
