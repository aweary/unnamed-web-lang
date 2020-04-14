#![warn(clippy::all)]

use diagnostics::ParseResult;
use hir::{Component, Module};

use std::sync::Arc;

pub fn codegen_module(_module: Module) -> ParseResult<()> {
    Ok(())
}

pub fn codegen_component(_compdef: Arc<Component>) {
}
