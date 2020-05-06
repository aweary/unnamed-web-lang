use crate::type_context::{Element, TypeContext};
use ::hir::unique_name::UniqueName;
use ::hir::*;

use ::hir::visit::{walk_function, Visitor};
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use ty::{boolean, number, Existential, LiteralType, Type, Variable};

use internment::Intern;
use log::debug;

type InternType = Intern<Type>;

/// Type checker. Starts from the root module and works
/// its way across the module graph.
pub struct TypeChecker {
    /// Ordered list of type elements
    context: TypeContext,
    /// Counter for creating new, unique existentials
    existential: u16,
    /// Counter for type variables
    variables: u16,
    /// The return type for the function we're currently type checking
    tracked_return_ty: Option<InternType>,
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
            StatementKind::Return(expr) => {
                debug!("return statement");
                let return_ty = self.tracked_return_ty.unwrap();
                self.checks_against(expr, return_ty)?;
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

        // Create a new unique name for the return type.
        let return_ty_name = UniqueName::new();

        // Read the return type
        let return_ty = match &function.ty {
            Some(ty) => {
                return Err(Diagnostic::error()
                    .with_message("Cant check annotations yet"))
            }
            None => {
                // New existential for the return type
                debug!("new existential for retutrn type");
                let alpha = self.fresh_existential();
                let existential = Element::new_existential(alpha);
                let existential_ty = Type::Existential(alpha).into();
                let element =
                    Element::new_typed_variable(return_ty_name, existential_ty);
                self.context.add(existential);
                self.context.add(element);
                existential_ty
            }
        };

        // Track it so we can refine it / check against it
        self.tracked_return_ty = Some(return_ty);

        walk_function(self, function)?;

        // Return type should be solved now
        let return_ty = self.apply_context(return_ty)?;

        debug!("complete visit_function: {:?}", function.name);
        debug!("return type: {:?}", return_ty);

        // Get the solved input types
        let mut input_ty = vec![];
        for param in &function.params {
            let name = param.unique_name;
            let ty = self.context.get_annotation(&name).unwrap();
            let ty = self.apply_context(ty)?;
            input_ty.push(ty)
        }

        let function_ty = Type::Function(input_ty, return_ty).into();

        let function_ty = self.apply_context(function_ty)?;

        debug!(
            "inferred function {:?} as {:#?}",
            function.name, function_ty
        );

        debug!("elements: {:#?}", self.context.elements);

        self.context.add(Element::new_typed_variable(
            function.unique_name,
            function_ty,
        ));

        Ok(())
    }
}

impl TypeChecker {
    pub fn new() -> Self {
        TypeChecker {
            context: TypeContext::default(),
            existential: 0,
            variables: 0,
            tracked_return_ty: None,
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

    /// Create a new type variable
    fn fresh_variable(&mut self) -> Variable {
        let id = self.variables;
        debug!("fresh_variable: {}", id);
        self.variables += 1;
        Variable(id)
    }

    /// Infer the type of some expression
    fn synth(&mut self, expr: &Expr) -> Result<InternType> {
        let t = self.synthesize(expr)?;
        let t = self.apply_context(t)?;
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
                    Binding::Parameter(param) => {
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
                let op_ty = Intern::new(self.bin_op_ty(op));
                let args = vec![&**left, &**right];
                let synth_ty = self.synthesize_application(op_ty, args)?;
                Ok(synth_ty)
            }
            ExprKind::Call(binding, args) => {
                // TODO mapping &Vec<Expr> to Vec<&Expr> is weird
                let mut mapped_args = vec![];
                for arg in args {
                    mapped_args.push(arg);
                }
                match binding {
                    // Calling a function definition
                    Binding::Function(function) => {
                        let function = function.lock().unwrap();
                        let name = function.unique_name;
                        let fn_ty = self.context.get_annotation(&name).unwrap();
                        let synth_ty =
                            self.synthesize_application(fn_ty, mapped_args)?;
                        Ok(synth_ty)
                    }
                    Binding::Parameter(param) => {
                        let name = param.unique_name;
                        let param_ty =
                            self.context.get_annotation(&name).unwrap();
                        let ty =
                            self.synthesize_application(param_ty, mapped_args)?;
                        Ok(ty)
                    }
                    _ => Err(Diagnostic::error()
                        .with_message("Cant call this type as a function")),
                }
            }
            _ => unimplemented!(""),
        }
    }

    fn bin_op_ty(&mut self, op: &BinOp) -> Type {
        match op {
            // Numeric operators
            BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod
            | BinOp::BinOr
            | BinOp::GreaterThan
            | BinOp::LessThan
            | BinOp::BinAdd => {
                Type::Function(vec![number!(), number!()], number!())
            }
            BinOp::DblEquals => {
                let v = self.fresh_variable();
                let ty_v = Intern::new(Type::Variable(v));
                Type::Quantification(
                    v,
                    Type::Function(vec![ty_v, ty_v], boolean!()).into(),
                )
            }
            // Others
            BinOp::Equals
            | BinOp::Add
            | BinOp::Sum
            | BinOp::And
            | BinOp::Or
            | BinOp::Pipeline => unimplemented!(),
        }
    }

    fn checks_against(&mut self, expr: &Expr, ty: InternType) -> Result<()> {
        debug!("check_against, {:?}", ty);
        // TODO assert is_well_formed
        match (&expr.kind, &*ty) {
            (ExprKind::Lit(_), Type::Literal(_)) => {
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
            // Subtyping
            (_, _) => {
                let a = self.synthesize(expr)?;
                let a = self.apply_context(a)?;
                let b = self.apply_context(ty)?;
                self.subtype(a, b).map_err(|err| {
                    err.with_labels(vec![Label::primary(expr.span)])
                })?;
                Ok(())
            }
        }
    }

    fn subtype(&mut self, a: InternType, b: InternType) -> Result<()> {
        debug!("subtype, a: {:?}, b: {:?}", a, b);
        match (&*a, &*b) {
            (Type::Literal(a), Type::Literal(b)) => {
                if a == b {
                    Ok(())
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "The literal types '{:?}' and '{:?}' are not compatible",
                        a, b
                    )))
                }
            }
            (Type::Variable(alpha), Type::Variable(beta)) => {
                if alpha == beta {
                    Ok(())
                } else {
                    panic!("Cant subtype type variables")
                }
            }
            (Type::Existential(alpha), _) => self.instantiate_l(*alpha, b),
            (_, Type::Existential(alpha)) => self.instantiate_r(*alpha, a),
            (_, _) => Err(Diagnostic::error().with_message("Cant subtype")),
        }
    }

    fn instantiate_l(
        &mut self,
        alpha: Existential,
        b: InternType,
    ) -> Result<()> {
        debug!("<:InstantiateL - {:?}", alpha);
        // First we need to split the context into left/right
        // TODO
        // let element = Element::new_existential(alpha);

        // Handle monotypes, TODO add well formed check
        if b.is_monotype() {
            // Solve this existential
            self.context.solve_existential(alpha, b)?;
            return Ok(());
        }
        Ok(())
    }

    fn instantiate_r(
        &mut self,
        alpha: Existential,
        a: InternType,
    ) -> Result<()> {
        debug!("<:InstantiateL - {:?}", alpha);
        // First we need to split the context into left/right
        // TODO
        // let element = Element::new_existential(alpha);

        // Handle monotypes, TODO add well formed check
        if a.is_monotype() {
            // Solve this existential
            self.context.solve_existential(alpha, a)?;
            return Ok(());
        }
        Ok(())
    }

    fn apply_context(&mut self, ty: InternType) -> Result<InternType> {
        debug!("apply_context {:?}", ty);
        match &*ty {
            Type::Existential(alpha) => {
                if let Some(tau) = self.context.get_solved(alpha) {
                    debug!("solved existential {:?} as {:?}", alpha, tau);
                    self.apply_context(tau)
                } else {
                    Ok(ty)
                }
            }
            Type::Function(inputs, output) => {
                // panic!("exit function apply_context");
                let mut mapped_inputs = vec![];
                for input in inputs {
                    mapped_inputs.push(self.apply_context(*input)?);
                }
                let output = self.apply_context(*output)?;
                Ok(Type::Function(mapped_inputs, output).into())
            }
            _ => Ok(ty),
        }
    }

    fn synthesize_application(
        &mut self,
        ty: InternType,
        args: Vec<&Expr>,
    ) -> Result<InternType> {
        debug!("synthesize_application: {:#?}", ty);
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
            Type::Quantification(v, ty) => {
                debug!("∀App");
                let alpha = self.fresh_existential();
                self.context.add(Element::new_existential(alpha));
                let substituted =
                    self.substitution(v, *ty, Type::Existential(alpha).into())?;
                self.synthesize_application(substituted, args)
            }
            Type::Existential(oldalpha) => {
                debug!("synthesize_application for existential {:?}", oldalpha);
                // Create an existential for the return type
                debug!("creating existential for return type");
                let alpha = self.fresh_existential();
                // Create an existential for every argument
                debug!("creating existentials for all arguments");
                let beta: Vec<Existential> =
                    args.iter().map(|_| self.fresh_existential()).collect();
                debug!("argument existentials: {:#?}", beta);
                // Create type elements for every new existential
                let beta_elements: Vec<Element> = beta
                    .iter()
                    .map(|alpha| Element::new_existential(*alpha))
                    .collect();
                let beta_ty : Vec<InternType> = beta
                    .iter()
                    .map(|alpha| Type::Existential(*alpha).into())
                    .collect();

                // Create an existential function type
                let fn_ty =
                    Type::Function(beta_ty.clone(), Type::Existential(alpha).into());
                debug!("created new function type: {:#?}", fn_ty);

                // Start with adding the existential for the return type
                let mut new_elements = vec![Element::new_existential(alpha)];

                // Include in all the new input existentials
                new_elements.extend(beta_elements);

                // Solve the existential
                new_elements.push(Element::new_solved(*oldalpha, fn_ty.into()));

                // Replace the old existential with all the new ones
                self.context.insert_in_place(
                    &Element::new_existential(*oldalpha),
                    new_elements,
                );

                for (expr, ty) in args.iter().zip(beta_ty) {
                    self.checks_against(expr, ty)?;
                }
                Ok(Type::Existential(alpha).into())
            }
            _ => todo!(),
        }
    }

    fn substitution(
        &self,
        alpha: &Variable,
        a: InternType,
        b: InternType,
    ) -> Result<InternType> {
        match &*a {
            Type::Literal(_) => Ok(a),
            Type::Variable(var) => {
                if var == alpha {
                    // Substitute it
                    return Ok(b);
                } else {
                    return Ok(a);
                }
            }
            Type::Quantification(var, ty) => {
                if var == alpha {
                    Ok(Type::Quantification(*var, b).into())
                } else {
                    Ok(Type::Quantification(
                        *var,
                        self.substitution(alpha, *ty, b)?,
                    )
                    .into())
                }
            }
            Type::Function(t1, t2) => {
                let mut s1 = vec![];
                for t in t1 {
                    s1.push(self.substitution(alpha, *t, b)?);
                }
                Ok(Type::Function(
                    s1.into(),
                    self.substitution(alpha, *t2, b)?.into(),
                )
                .into())
                // let mut t1 = vec![];
            }
            Type::Existential(var) => {
                // In what case would we replace a type variable with an existential?
                Ok(a)
            }
            Type::Pair(t1, t2) => Ok(Type::Pair(
                self.substitution(alpha, *t1, b)?,
                self.substitution(alpha, *t2, b)?,
            )
            .into()),
            _ => unimplemented!(),
        }
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
