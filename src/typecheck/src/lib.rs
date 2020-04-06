use ::hir::*;
use data_structures::module_graph::ModuleGraph;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use source::filesystem::FileId;
use syntax::symbol::Symbol;

use ::hir::visit::Visitor;

use std::sync::{Arc, Mutex};

pub struct TyCtx {
    file: FileId,
    module_graph: Arc<Mutex<ModuleGraph<Module, Symbol>>>,
    // The number of existentials
    existentials: u16,
    // Ordered list of type elements
    elements: Vec<TyCtxElement>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct Existential(u16);

impl Visitor for TyCtx {
    fn visit_function(&mut self, func: &Function) -> Result<()> {
        let name = &func.name;
        let params = &func.params;
        println!("Visiting function! {:#?}", params);
        Ok(())
        // ...
    }
}

impl TyCtx {
    pub fn new(file: FileId, module_graph: Arc<Mutex<ModuleGraph<Module, Symbol>>>) -> Self {
        TyCtx {
            file,
            module_graph,
            existentials: 0,
            elements: vec![],
        }
    }

    /// Start checking the program from the root module, following imports.
    /// This will look for the `main` function in the module to get started.
    pub fn check_from_root(&mut self, root_module: &Module) -> Result<()> {
        self.visit_module(root_module)?;
        // Err(Diagnostic::error().with_message("Type checking function failed!"))
        // First we need to iterate through all the module's definitions
        // and populate the type context with the type definitions and signatures of
        // the functions and components.
        Ok(())
    }

    /// Synthesis a type from an expression
    fn synth(&mut self, expr: &Expr) -> Result<Ty> {
        let t = self.snythesizes_to(&expr)?;
        let t = self.apply_context(t)?;
        Ok(t)
    }

    fn snythesizes_to(&mut self, expr: &Expr) -> Result<Ty> {
        use ExprKind::*;
        match &expr.kind {
            // Literal types
            ExprKind::Lit(lit) => Ok(Ty::Lit(self.literal_synthesizes_to(&lit))),
            // References
            ExprKind::Reference(ident, _binding) => {
                let t = self.get_annotation(&ident)?;
                Ok(t.clone())
            }
            // Tuples
            ExprKind::Tuple(exprs) => {
                let mut t = vec![];
                for expr in exprs {
                    t.push(self.snythesizes_to(expr)?)
                }
                Ok(Ty::Product(t))
            }
            // Function Application
            ExprKind::Call(binding, arguments) => {
                Err(Diagnostic::error().with_message("Type synthesis failed!"))
            }
            ExprKind::Lambda(lambda) => {
                Err(Diagnostic::error().with_message("Type synthesis failed!"))
            }
            // Annotations? Need to do that somewhere else as annotaitons
            // aren't expressions in this language
            // ...
            // Check functions defined as expressions
            // Abstraction is (mainly) done iterating through a module's definitions as well...
            ExprKind::Func(func) => {
                let func = func.lock().unwrap();
                Ok(Ty::Variable)
            }
            _ => Err(Diagnostic::error().with_message("Type synthesis failed!")),
        }
    }

    fn apply_context(&mut self, t: Ty) -> Result<Ty> {
        Ok(t)
    }

    /// Infer the type of a literal value.
    fn literal_synthesizes_to(&self, lit: &Lit) -> LitTy {
        match lit.kind {
            LitKind::Bool(_) => LitTy::Boolean,
            LitKind::Str(_) => LitTy::String,
            LitKind::Number(_) => LitTy::Number,
        }
    }

    fn get_annotation(&self, reference: &Ident) -> Result<&Ty> {
        for element in &self.elements {
            if let TyCtxElement::TypedVariable(symbol, ty) = element {
                if symbol == &reference.name {
                    return Ok(ty);
                }
            }
        }
        Err(Diagnostic::error().with_message("Unable to resolve annotation for reference"))
    }

    fn fresh_existential(&mut self) -> Existential {
        let e = Existential(self.existentials);
        self.existentials += 1;
        e
    }

    fn is_well_formed(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Lit(_) => true,
            _ => true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TyCtxElement {
    Variable(Symbol),
    Existential(Existential),
    Solved(Symbol, Ty),
    Marker(String),
    TypedVariable(Symbol, Ty),
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    Lit(LitTy),
    // The abstraction type is what we use for functions and components.
    // It has a single input and output type. Functions with multiple arguments
    // use a Product type in the input position
    Abstraction(Box<Ty>, Box<Ty>),
    // TODO is this a type variable, or the variable of a type?
    Variable,
    Product(Vec<Ty>),
    Existential(String),
    Quantification(Symbol, Box<Ty>),
}

/// Literal value types. These are primitive types
/// that we know a priori in the type system
#[derive(Debug, Clone, PartialEq, Eq)]
enum LitTy {
    String,
    Number,
    Boolean,
}
