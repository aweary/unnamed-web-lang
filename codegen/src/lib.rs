#![warn(clippy::all)]

use diagnostics::ParseResult;
use hir::{Component, Module};

use std::sync::Arc;

pub fn codegen_module(_module: Module) -> ParseResult<()> {
    // for def in module.definitions {
    //     match def.kind {
    //         hir::DefinitionKind::Component(compdef) => {
    //             codegen_component(compdef);
    //             // println!("codegen component...");
    //             // ...
    //         }
    //         _ => {
    //             // ...
    //         }
    //     }
    // }
    // println!("Codegen'ing a module");
    Ok(())
}

pub fn codegen_component(_compdef: Arc<Component>) {
    println!("codegen component?");
}
