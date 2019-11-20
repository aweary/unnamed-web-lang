#![warn(clippy::all)]

pub mod diagnostics;
pub mod error;
pub use codespan::*;
pub use codespan_reporting::diagnostic::*;
pub use diagnostics::*;
