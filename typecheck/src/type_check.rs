use ::hir::*;
use crate::type_context::{Element, TypeContext};

use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use ::hir::visit::{walk_function, Visitor};
use ty::{number, Existential, LiteralType, Type};

use internment::Intern;
use log::debug;

type InternType = Intern<Type>;

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
        debug!("visit_statement");
        match &statement.kind {
            StatementKind::Local(local) => {
                debug!("checking local: {:?}", local.name);
                let Local {
                    unique_name, init, ..
                } = &**local;
                // TODO handle cases where there is no `init`. Maybe we should
                // require initialization?
                if let Some(expr) = init {
                    let ty = self.synth(&*expr)?;
                    debug!("synth local type: {:?}", ty);
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

    fn visit_function(&mut self, function: &hir::Function) -> Result<()> {
        debug!("visit_function: {:?}", function.name);
        for param in &function.params {
            debug!("check param: {:?}", param.local);
            let name = param.unique_name;
            if let Some(ty) = &param.ty {
                match ty.kind {
                    _ => {
                        return Err(Diagnostic::error()
                            .with_message("Cant type check annotations yet")
                            .with_labels(vec![Label::primary(param.span)]));
                        // TODO
                        // let element = Element::new_typed_variable(name, param.ty.clone());
                        // self.context.add(element);
                    }
                }
            } else {
                debug!("need existential for param `{:?}`", param.local);
                // No type annotation means we need to infer
                let alpha = self.fresh_existential();
                let existential = Element::new_existential(alpha);
                let element = Element::new_typed_variable(
                    name,
                    Intern::new(Type::Existential(alpha)),
                );
                self.context.add(existential);
                self.context.add(element);
            }
        }
        walk_function(self, function)?;
        debug!("complete visit_function: {:?}", function.name);
        Ok(())
    }
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
        debug!("fresh_existential: {}", id);
        self.existential += 1;
        Existential(id)
    }

    /// Infer the type of some expression
    fn synth(&mut self, expr: &Expr) -> Result<InternType> {
        let t = self.synthesize(expr)?;
        // TODO apply context
        Ok(t)
    }

    /// Infer the type of some expression, before applying the context
    fn synthesize(&mut self, expr: &Expr) -> Result<InternType> {
        match &expr.kind {
            ExprKind::Lit(lit) => Ok(infer_literal(lit)),
            ExprKind::Reference(_ident, binding) => {
                match binding {
                    Binding::Local(local) => {
                        let name = local.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        Ok(ty)
                    }
                    Binding::Argument(param) => {
                        let name = param.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        Ok(ty)
                    }
                    Binding::State(_)
                    | Binding::Function(_)
                    | Binding::Component(_)
                    | Binding::Import(_)
                    | Binding::Constant(_)
                    | Binding::Type(_)
                    | Binding::Enum(_)
                    | Binding::Wildcard => Err(Diagnostic::error()
                        .with_message("Cant check these references yet")),
                }
                // Get the unique name for this binding
            }
            ExprKind::Binary(op, left, right) => {
                // Synthesize a function type for the binary operator
                let op_ty = Intern::new(bin_op_ty(op));
                let args = vec![&**left, &**right];
                let synth_ty = self.synthesize_application(op_ty, args).map_err(|diagnostic| {
                    diagnostic.with_message(
                        format!("Both sides of a arithmetic operator must be a number")
                    )
                })?;
                todo!()
            }
            _ => unimplemented!(""),
        }
    }

    fn checks_against(&mut self, expr: &Expr, ty: InternType) -> Result<()> {
        // TODO assert is_well_formed
        match (&expr.kind, &*ty) {
            (ExprKind::Lit(lit), Type::Literal(lit_ty)) => {
                // Check that literal types are equal
                let synth_ty = self.synth(expr)?;
                if ty == synth_ty {
                    Ok(())
                } else {
                    let msg = format!("Argument of type '{:?}' is not assignable to parameter of type '{:?}'", synth_ty, ty);
                    Err(Diagnostic::error()
                        .with_message(msg)
                        .with_labels(vec![Label::primary(expr.span)]))
                }
            }
            _ => Err(Diagnostic::error().with_message("Cant check types")),
        }
    }

    pub fn synthesize_application(
        &mut self,
        ty: InternType,
        args: Vec<&Expr>,
    ) -> Result<InternType> {
        match &*ty {
            Type::Function(inputs, output) => {
                // TODO error handling for too many/few arguments
                assert_eq!(inputs.len(), args.len());
                // Check the arguments against the input types
                for (ty, expr) in inputs.iter().zip(args) {
                    self.checks_against(expr, *ty)?;
                }
                // If everything checks, return the output type
                Ok(*output)
            }
            _ => todo!(),
        }
    }
}

fn bin_op_ty(op: &BinOp) -> Type {
    match op {
        // Numeric operators
        BinOp::Sub
        | BinOp::Mul
        | BinOp::Div
        | BinOp::Mod
        | BinOp::BinOr
        | BinOp::BinAdd => {
            Type::Function(vec![number!(), number!()], number!())
        }
        // Others
        BinOp::Equals
        | BinOp::DblEquals
        | BinOp::Add
        | BinOp::Sum
        | BinOp::And
        | BinOp::Or
        | BinOp::GreaterThan
        | BinOp::LessThan
        | BinOp::Pipeline => unimplemented!(),
    }
}

/// Infers the type of a literal expression
fn infer_literal(lit: &Lit) -> InternType {
    let lit_ty = match lit.kind {
        LitKind::Bool(_) => LiteralType::Boolean,
        LitKind::Str(_) => LiteralType::String,
        LitKind::Number(_) => LiteralType::Number,
    };
    Intern::new(Type::Literal(lit_ty))
}
