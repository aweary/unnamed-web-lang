#![warn(clippy::all)]

use diagnostics::ParseResult;
use hir::{Module, Component};

use std::sync::Arc;

pub fn codegen_module(module: Module) -> ParseResult<()> {
    for def in module.definitions {
        match def.kind {
            hir::DefinitionKind::Component(compdef) => {
                codegen_component(compdef);
                // println!("codegen component...");
                // ...
            }
            _ => {
                // ...
            }
        }
    }
    // println!("Codegen'ing a module");
    Ok(())
}

fn codegen_component(compdef: Arc<Component>) {
    println!("codegen component?");
}
