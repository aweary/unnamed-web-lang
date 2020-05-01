use ::hir::*;
use data_structures::module_graph::ModuleGraph;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use source::filesystem::FileId;
use syntax::symbol::Symbol;

use std::collections::HashMap;

use ::hir::visit::{self, walk_block, walk_function, Visitor};

use crate::type_context::{Element, TypeContext};
use ty::{Existential, LiteralType, Type};

use std::sync::{Arc, Mutex};

/// Type checker. Starts from the root module and works
/// its way across the module graph.
pub struct TypeChecker {
    /// Ordered list of type elements
    context: TypeContext,
    /// Monotonic counter for creating new, unique existentials
    existential: u16,
}

impl Visitor for TypeChecker {
    fn visit_statement(&mut self, statement: &hir::Statement) -> Result<()> {
        match &statement.kind {
            StatementKind::Local(local) => {
                let Local {
                    unique_name, init, ..
                } = &**local;
                // TODO handle cases where there is no `init`. Maybe we should
                // require initialization?
                if let Some(expr) = init {
                    let ty = self.synth(&*expr)?;
                    let element = Element::new_typed_variable(*unique_name, ty);
                    self.context.add(element);
                }
            }
            _ => {}
        }
        Ok(())
    }
    // fn visit_import(&mut self, _import: &hir::Import) -> Result<()> {
    //     Ok(())
    // }

    // fn visit_function(&mut self, _function: &hir::Function) -> Result<()> {
    //     Ok(())
    // }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            context: TypeContext::default(),
            existential: 0,
        }
    }

    /// Run the type checker from a root HIR module.
    pub fn run(&mut self, module: &hir::Module) -> Result<()> {
        self.visit_module(module)?;
        Ok(())
    }

    /// Create a new existential
    fn fresh_existential(&mut self) -> Existential {
        let id = self.existential;
        self.existential += 1;
        Existential(id)
    }

    /// Infer the type of some expression
    fn synth(&mut self, expr: &Expr) -> Result<Type> {
        let t = self.synthesize(expr)?;
        unimplemented!()
    }

    /// Infer the type of some expression, before applying the context
    fn synthesize(&mut self, expr: &Expr) -> Result<Type> {
        match &expr.kind {
            ExprKind::Lit(lit) => Ok(infer_literal(lit)),
            ExprKind::Reference(_ident, _binding) => {
                // Get the unique name for this binding
                Err(Diagnostic::error().with_message("Cant synth reference"))
            },
            _ => unimplemented!(""),
        }
    }
}

/// Infers the type of a literal expression
fn infer_literal(lit: &Lit) -> Type {
    let lit_ty = match lit.kind {
        LitKind::Bool(_) => LiteralType::Boolean,
        LitKind::Str(_) => LiteralType::String,
        LitKind::Number(_) => LiteralType::Number,
    };
    Type::Literal(lit_ty)
}
