use crate::hir::*;
use diagnostics::ParseResult as Result;

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
    fn visit_component(&mut self, _fndef: &Component) -> Result<()> {
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

    fn visit_type(&mut self, _alias: &Type) -> Result<()> {
        Ok(())
    }

    fn visit_type_alias(&mut self, _: &TypeAlias) -> Result<()> {
        Ok(())
    }

    fn visit_lambda(&mut self, lambda: &Lambda) -> Result<()> {
        walk_lambda(self, lambda)?;
        Ok(())
    }

    fn visit_local(&mut self, local: &Arc<Local>) -> Result<()> {
        if let Some(expr) = &local.init {
            walk_expr(self, expr)?;
        }
        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<()> {
        Ok(())
    }
}

pub fn walk_lambda<V: Visitor>(visitor: &mut V, lambda: &Lambda) -> Result<()> {
    // let Lambda { params, body, .. } = lambda;
    Ok(())
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, expr: &Expr) -> Result<()> {
    visitor.visit_expr(expr)?;
    match &expr.kind {
        crate::ExprKind::Lambda(lambda) => {
            visitor.visit_lambda(lambda)?;
        }
        crate::ExprKind::Array(_) => {}
        crate::ExprKind::Object(_) => {}
        crate::ExprKind::Tuple(_) => {}
        crate::ExprKind::Block(_) => {}
        crate::ExprKind::Binary(_, _, _) => {}
        crate::ExprKind::Unary(_, _) => {}
        crate::ExprKind::Cond(_, _, _) => {}
        crate::ExprKind::Call(_, _) => {}
        crate::ExprKind::MemberCall(_, _, _) => {}
        crate::ExprKind::Assign(_, _, _) => {}
        crate::ExprKind::StateUpdate(_, _, _) => {}
        crate::ExprKind::Member(_, _) => {}
        crate::ExprKind::OptionalMember(_, _) => {}
        crate::ExprKind::Lit(_) => {}
        crate::ExprKind::Reference(_, _) => {}
        crate::ExprKind::If(_) => {}
        crate::ExprKind::For(_, _, _) => {}
        crate::ExprKind::Index(_, _) => {}
        crate::ExprKind::Return(_) => {}
        crate::ExprKind::Template(_) => {}
        crate::ExprKind::Match(_, _) => {}
        crate::ExprKind::Func(_) => {}
        crate::ExprKind::TrailingClosure(_, _) => {}
    }
    Ok(())
}

pub fn walk_statement<V: Visitor>(
    _visitor: &mut V,
    _statement: &Statement,
) -> Result<()> {
    {
        // ...
    };
    Ok(())
}

pub fn walk_definition<V: Visitor>(
    visitor: &mut V,
    definition: &Definition,
) -> Result<()> {
    match &definition.kind {
        DefinitionKind::Function(fndef) => {
            let mut fndef = fndef.lock().unwrap();
            let fndef = &mut *fndef;
            visitor.visit_function(fndef)?;
        }
        DefinitionKind::Component(compdef) => {
            visitor.visit_component(compdef)?;
        }
        DefinitionKind::Type(ty) => {
            // TODO
            visitor.visit_type_alias(ty)?;
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
        visitor.visit_statement(statement)?;
        match &statement.kind {
            crate::StatementKind::Local(local) => {
                visitor.visit_local(&local)?;
            }
            crate::StatementKind::State(_) => {}
            crate::StatementKind::Expr(_) => {}
            crate::StatementKind::BranchingCondition => {}
            crate::StatementKind::LoopingCondition => {}
            crate::StatementKind::Return(_) => {}
            crate::StatementKind::If(_) => {}
            crate::StatementKind::Throw(_) => {}
        }
    }
    Ok(())
}

pub fn walk_component<V: Visitor>(
    visitor: &mut V,
    component: &Component,
) -> Result<()> {
    // TODO walk params / function header stuff
    visitor.visit_block(&component.body)?;
    Ok(())
}

pub fn walk_function<V: Visitor>(
    visitor: &mut V,
    fndef: &Function,
) -> Result<()> {
    // TODO walk params / function header stuff
    visitor.visit_block(&fndef.body)?;
    Ok(())
}

pub fn walk_module<V: Visitor>(visitor: &mut V, module: &Module) -> Result<()> {
    for import in &module.imports {
        let mut import = import.lock().unwrap();
        let import = &mut *import;
        visitor.visit_import(import)?;
    }
    for definition in &module.definitions {
        visitor.visit_definition(&definition)?;
    }
    Ok(())
}
