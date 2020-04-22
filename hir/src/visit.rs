use crate::hir::*;
use diagnostics::ParseResult as Result;
use std::ops::DerefMut;
use std::sync::Arc;

pub trait Visitor: Sized {
    fn visit_module(&mut self, module: &Module) -> Result<()> {
        walk_module(self, module)?;
        Ok(())
    }

    fn visit_definition(&mut self, definition: &Definition) -> Result<()> {
        walk_definition(self, definition)?;
        Ok(())
        // ...
    }

    fn visit_import(&mut self, _import: &Import) -> Result<()> {
        Ok(())
        // ...
    }

    fn visit_function(&mut self, fndef: &Function) -> Result<()> {
        walk_function(self, fndef)?;
        Ok(())
    }
    fn visit_component(&mut self, _fndef: &Arc<Component>) -> Result<()> {
        Ok(())
        // ...
    }

    fn visit_block(&mut self, block: &Block) -> Result<()> {
        walk_block(self, block)
    }

    fn visit_statement(&mut self, statement: &Statement) -> Result<()> {
        walk_statement(self, statement)?;
        Ok(())
    }
}

pub fn walk_statement<V: Visitor>(_visitor: &mut V, statement: &Statement) -> Result<()> {
    match statement.kind {
        _ => {
            // ...
        }
    };
    Ok(())
}

pub fn walk_definition<V: Visitor>(visitor: &mut V, definition: &Definition) -> Result<()> {
    match &definition.kind {
        DefinitionKind::Function(fndef) => {
            let mut fndef = fndef.lock().unwrap();
            let fndef = fndef.deref_mut();
            visitor.visit_function(fndef)?;
        }
        DefinitionKind::Component(compdef) => {
            visitor.visit_component(compdef)?;
        }
        // DefinitionKind::Import(imports) => {
        // }
        _ => {
            // ..
        }
    };
    Ok(())
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block: &Block) -> Result<()> {
    for statement in &block.statements {
        let mut statement = statement.lock().unwrap();
        let statement = statement.deref_mut();
        visitor.visit_statement(statement)?;
    }
    Ok(())
}

pub fn walk_function<V: Visitor>(visitor: &mut V, fndef: &Function) -> Result<()> {
    // TODO walk params / function header stuff
    visitor.visit_block(&fndef.body)?;
    Ok(())
}

pub fn walk_module<V: Visitor>(visitor: &mut V, module: &Module) -> Result<()> {
    for import in &module.imports {
        let mut import = import.lock().unwrap();
        let import = import.deref_mut();
        visitor.visit_import(import)?;
    }
    for definition in &module.definitions {
        visitor.visit_definition(&definition)?;
    }
    Ok(())
}
