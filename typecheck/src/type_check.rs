use crate::type_context::{Element, TypeContext};
use ::hir::unique_name::UniqueName;
use ::hir::{
    hir, BinOp, Binding, Expr, ExprKind, Lit, LitKind, Local, StatementKind,
    Type as HIRType,
};

use ::hir::visit::{walk_block, walk_component, walk_function, Visitor};
use data_structures::HashMap;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use ty::effects::{EffectConstant, EffectType};
use ty::{boolean, number, string, Existential, LiteralType, Type, Variable};

use internment::Intern;
use log::{debug, info};

use std::sync::Arc;
use std::time::Instant;

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
    /// A set of effects being tracked for the function we're currently checking
    tracked_effect_ty: Option<EffectType>,
    /// Maps unique names to type variables
    variable_map: HashMap<UniqueName, Variable>,
}

impl Visitor for TypeChecker {
    fn visit_statement(&mut self, statement: &hir::Statement) -> Result<()> {
        debug!("visit_statement");
        match &statement.kind {
            StatementKind::Expr(expr) => {
                // Just check the expression can be inferred
                self.synth(expr)?;
            }
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
            StatementKind::If(ifexpr) => {
                let hir::IfExpr {
                    condition,
                    block,
                    alt,
                    ..
                } = ifexpr;

                // Check that the condition evaluates to a boolean
                self.checks_against(&*condition, boolean!())?;

                // TODO could we check if this condition will *always* evaluate
                // to `true` or `false`? If we can statically exclude the easy cases
                // maybe we can give better effect inference.

                walk_block(self, block)?;
            }
            StatementKind::State(local) => {
                // A state variable introduces a state effect into the effect row.
                let tracked_effect_ty =
                    self.tracked_effect_ty.as_mut().unwrap();
                tracked_effect_ty.add(EffectConstant::State);
                // TODO dedupe with above branch
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
            StatementKind::Throw(expr) => {
                debug!("visit throw statement");
                let ty = self.synth(expr)?;
                debug!("throwing a {:?}", ty);
                let effect = EffectConstant::Exn(ty);
                let tracked_effect_ty =
                    self.tracked_effect_ty.as_mut().unwrap();
                tracked_effect_ty.add(effect);
                // Add this to the effect row for the current function
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

    fn visit_enum_def(&mut self, enum_def: &hir::EnumDef) -> Result<()> {
        debug!("visit_enum_def");
        // Create the ADT type for the enum definition, add it to context
        let hir::EnumDef {
            name,
            unique_name,
            variants,
            parameters,
            span,
        } = &*enum_def;
        let mut ids = vec![];
        for hir::Variant {
            unique_name, ident, ..
        } in variants
        {
            let id: u32 = (*unique_name).into();
            ids.push(id);
            // ...
        }
        let ty: InternType = Type::Adt { variants: ids }.into();
        self.context
            .add(Element::new_typed_variable(*unique_name, ty));
        Ok(())
    }

    fn visit_type_alias(&mut self, type_alias: &hir::TypeAlias) -> Result<()> {
        debug!("visit_type_alias, {:#?}", type_alias);
        let hir::TypeAlias {
            unique_name,
            parameters,
            ty,
            ..
        } = type_alias;

        // If there are parameters we create type variables for them
        // and map them back to the unique name of the type parameter.
        //
        // TODO do we need to do this when visiting type definitions? Maybe
        // it should happen when type application occurs
        let parameters = parameters.as_ref().map(|parameters| {
            let mut tvars = vec![];
            for hir::TVar { unique_name, .. } in parameters {
                let tvar = self.fresh_variable();
                debug!("Map variable {:?} to tvar {:?}", unique_name, tvar);
                self.variable_map.insert(*unique_name, tvar);
                tvars.push(tvar);
            }
            tvars
        });

        let mut ty = self.hir_type_to_type(ty)?;

        if let Some(tvars) = parameters {
            ty = Type::Quantification(tvars, ty).into();
        }

        debug!("Type for {:?} is {:#?}", unique_name, ty);

        self.context
            .add(Element::new_typed_variable(*unique_name, ty));
        Ok(())
    }

    fn visit_component(&mut self, component: &hir::Component) -> Result<()> {
        debug!("visit_component");
        self.add_fn_params_to_context(&component.params)?;
        // Create a new unique name for the return type.
        let return_ty_name = UniqueName::new();

        // Read the return type
        let return_ty = match &component.ty {
            Some(ty) => {
                let _name = component.unique_name;
                self.hir_type_to_type(ty)?
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

        walk_component(self, component)?;

        Ok(())
    }

    fn visit_function(&mut self, function: &hir::Function) -> Result<()> {
        // If there are generics, we need to treat this as a quantification type
        let tvars = if let Some(tvars) = &function.generics {
            let mut variables = vec![];
            for tvar in tvars {
                let variable = self.fresh_variable();
                self.variable_map
                    .entry(tvar.unique_name)
                    .or_insert(variable);
                variables.push(variable)
            }
            Some(variables)
        } else {
            None
        };

        debug!("visit_function: {:?}", function.name);
        self.add_fn_params_to_context(&function.params)?;
        // Create a new unique name for the return type.
        let return_ty_name = UniqueName::new();

        // Read the return type
        let return_ty = match &function.ty {
            Some(ty) => {
                let _name = function.unique_name;
                self.hir_type_to_type(ty)?
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

        let effect_ty = EffectType::default();

        // Track it so we can refine it / check against it
        self.tracked_return_ty = Some(return_ty);
        self.tracked_effect_ty = Some(effect_ty);

        walk_function(self, function)?;

        // Return type should be solved now
        let return_ty = self.apply_context(return_ty)?;

        // Effect row should be populated

        // if there were no return statements, check the value of the last statement

        let input_ty = self.synth_fn_params(&function.params)?;

        debug!("complete visit_function: {:?}", function.name);
        debug!("return type: {:?}", return_ty);
        let effect_ty = self.tracked_effect_ty.as_ref().unwrap().clone();
        debug!("effect_ty type: {:?}", effect_ty);

        let function_ty = self.apply_context(
            Type::new_function_with_effect(input_ty, return_ty, effect_ty),
        )?;

        let function_ty = if let Some(tvars) = tvars {
            Type::new_quantification(tvars, function_ty)
        } else {
            function_ty
        };

        debug!(
            "inferred function {:?} as {:#?}",
            function.name, function_ty
        );

        self.context.add(Element::new_typed_variable(
            function.unique_name,
            function_ty,
        ));

        Ok(())
    }
}

impl TypeChecker {
    #[must_use]
    pub fn new() -> Self {
        return Self {
            context: TypeContext::default(),
            existential: 0,
            variables: 0,
            tracked_return_ty: None,
            tracked_effect_ty: None,
            variable_map: HashMap::default(),
        };
    }

    /// Run the type checker from a root HIR module.
    pub fn run(&mut self, module: &hir::Module) -> Result<()> {
        let start = Instant::now();
        match self.visit_module(module) {
            Ok(_) => {
                let end = Instant::now();
                let duration = end.duration_since(start);
                info!(
                    "Type checking module took: {:?}us",
                    duration.as_micros()
                );
                Ok(())
            }
            Err(err) => {
                let end = Instant::now();
                let duration = end.duration_since(start);
                info!(
                    "Type checking module took: {:?}us",
                    duration.as_micros()
                );
                Err(err)
            }
        }
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

    fn add_fn_params_to_context(
        &mut self,
        params: &Vec<Arc<hir::Param>>,
    ) -> Result<()> {
        debug!("add_fn_params_to_context");
        for param in params {
            debug!("check param: {:?}, {:?}", param.local, param.unique_name);
            let name = param.unique_name;
            if let Some(ty) = &param.ty {
                let ty = self.hir_type_to_type(ty)?;
                let element = Element::new_typed_variable(name, ty);
                self.context.add(element);
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
        Ok(())
    }

    fn synth_fn_params(
        &mut self,
        params: &Vec<Arc<hir::Param>>,
    ) -> Result<Vec<ty::Parameter>> {
        // Get the solved input types
        let mut input_ty = vec![];
        for param in params {
            let symbol: syntax::symbol::Symbol = param.local.clone().into();
            let name = param.unique_name;
            let ty = self.context.get_annotation(&name).unwrap();
            let ty = self.apply_context(ty)?;
            let param_ty = ty::Parameter {
                name: Some(symbol),
                ty,
            };
            input_ty.push(param_ty)
        }
        Ok(input_ty)
    }

    /// Infer the type of some expression, before applying the context
    fn synthesize(&mut self, expr: &Expr) -> Result<InternType> {
        debug!("synthesize");
        match &expr.kind {
            ExprKind::Lit(lit) => Ok(infer_literal(lit)),
            ExprKind::Lambda(lambda) => {
                debug!("synthesize lambda");
                let hir::Lambda {
                    params,
                    body,
                    span: _,
                    ..
                } = lambda;

                self.add_fn_params_to_context(params)?;

                let return_ty = match body {
                    // Walk the lambda body block and track the return types
                    hir::LambdaBody::Block(block) => {
                        // Cache the last tracked return type
                        let cached_tracked_return_ty = self.tracked_return_ty;
                        // Set a new one, for this lambda.
                        // TODO check annotations for lambdas
                        let return_ty = {
                            let return_ty_name = UniqueName::new();
                            // New existential for the return type
                            // TODO dedupe this with function declarations
                            let alpha = self.fresh_existential();
                            let existential = Element::new_existential(alpha);
                            let existential_ty =
                                Type::Existential(alpha).into();
                            let element = Element::new_typed_variable(
                                return_ty_name,
                                existential_ty,
                            );
                            self.context.add(existential);
                            self.context.add(element);
                            existential_ty
                        };

                        self.tracked_return_ty = Some(return_ty);
                        self.visit_block(&*block)?;
                        let ty = self.apply_context(return_ty)?;
                        self.tracked_return_ty = cached_tracked_return_ty;
                        ty
                    }
                    hir::LambdaBody::Expr(expr) => {
                        self.synth(expr)?
                        // Infer the type of this expression
                    }
                };

                let input_ty = self.synth_fn_params(&params)?;
                let ty = Type::new_function(input_ty, return_ty);
                debug!("Lambda type: {:#?}", ty);
                Ok(ty)
                // Err(Diagnostic::error()
                //     .with_message("Cant infer lambdas right now"))
            }
            ExprKind::Reference(_ident, binding) => {
                debug!("synthesizing a reference!");
                match binding {
                    Binding::Local(local) | Binding::State(local) => {
                        let name = local.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        Ok(ty)
                    }
                    Binding::Parameter(param) => {
                        let name = param.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        debug!(
                            "synthesize a reference to a paramter: {:?}",
                            ty
                        );
                        Ok(ty)
                    }
                    Binding::TypeParameter(unique_name) => {
                        let ty =
                            self.context.get_annotation(&unique_name).unwrap();
                        Ok(ty)
                    }
                    Binding::Function(function) => {
                        let name = function.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        Ok(ty)
                    }
                    Binding::Component(_)
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

                // TODO don't clone both expressions everytime!
                let op_ty = self.bin_op_ty(op);
                let left = hir::Argument {
                    span: left.span,
                    value: *left.clone(),
                    name: None,
                };
                let right = hir::Argument {
                    span: right.span,
                    value: *right.clone(),
                    name: None,
                };
                let args = vec![&left, &right];
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
                        let name = function.unique_name;
                        let fn_ty = self.context.get_annotation(&name).unwrap();

                        // Check if we're using positional arguments and validate that they're
                        // complete and correct.

                        // Add this effect to the tracked effects
                        // TODO.
                        // We should have another way of tying effects and types
                        // We shouldn't depend on only the Function case itself to hold
                        // the type. It should be opaque. Maybe interned too.

                        let synth_ty =
                            self.synthesize_application(fn_ty, mapped_args)?;
                        Ok(synth_ty)
                    }
                    Binding::Parameter(param) => {
                        let name = param.unique_name;
                        let param_ty =
                            self.context.get_annotation(&name).unwrap();
                        debug!("calling parameter: {:#?}", param_ty);
                        let ty =
                            self.synthesize_application(param_ty, mapped_args)?;
                        Ok(ty)
                    }
                    Binding::Local(local) | Binding::State(local) => {
                        let name = local.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        self.synthesize_application(ty, mapped_args)
                    }
                    Binding::Import(import) => {
                        use std::ops::Deref;
                        let import = import.lock().unwrap();
                        let hir::Import { name, span, .. } = import.deref();
                        Err(Diagnostic::error()
                            .with_message("Cant type check imports yet")
                            .with_labels(vec![
                                Label::primary(name.span),
                                Label::secondary(*span),
                            ]))
                    }
                    _ => Err(Diagnostic::error()
                        .with_message("Cant call this type as a function")),
                }
            }
            ExprKind::TrailingClosure(a, b) => {
                todo!("cant synth type for trailing closure expressions");
            }
            ExprKind::EnumVariant(enumdef, variant) => {
                let ty =
                    self.context.get_annotation(&enumdef.unique_name).unwrap();
                
                todo!("ENUM VARIANT");
            }
            _ => unimplemented!(""),
        }
    }

    fn bin_op_ty(&mut self, op: &BinOp) -> InternType {
        match op {
            // Numeric operators
            BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod
            | BinOp::BinOr
            | BinOp::GreaterThan
            | BinOp::LessThan
            | BinOp::BinAdd => Type::new_function(
                vec![
                    ty::Parameter {
                        ty: number!(),
                        name: None,
                    },
                    ty::Parameter {
                        ty: number!(),
                        name: None,
                    },
                ],
                number!(),
            ),
            BinOp::DblEquals => {
                let v = self.fresh_variable();
                let ty_v = Intern::new(Type::Variable(v));
                Type::Quantification(
                    vec![v],
                    Type::new_function(
                        vec![
                            ty::Parameter {
                                ty: ty_v,
                                name: None,
                            },
                            ty::Parameter {
                                ty: ty_v,
                                name: None,
                            },
                        ],
                        boolean!(),
                    ),
                )
                .into()
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
        debug!("check_against\nexpr: {:#?}\nty: {:#?}", expr, ty);
        // TODO assert is_well_formed
        match (&expr.kind, &*ty) {
            // I1
            (ExprKind::Lit(_), Type::Literal(_)) => {
                debug!("1I");
                // Check that literal types are equal
                let synth_ty = self.synth(expr)?;
                if ty == synth_ty {
                    Ok(())
                } else {
                    let msg = format!("Argument of type '{}' is not assignable to parameter of type '{}'", synth_ty, ty);
                    Err(Diagnostic::error()
                        .with_message(msg)
                        .with_labels(vec![Label::primary(expr.span)]))
                }
            }
            //->I, lambdas
            (ExprKind::Lambda(lambda), Type::Function { parameters, .. }) => {
                let hir::Lambda {
                    params, span: _, ..
                } = lambda;
                assert_eq!(params.len(), parameters.len());
                debug!("->I");
                Ok(())
            }
            //forallI
            (_, Type::Quantification(alphas, a)) => {
                debug!("\u{2200}I");
                for alpha in alphas {
                    let var = Element::new_variable(*alpha);
                    self.context.add(var.clone());
                    self.checks_against(expr, *a)?;
                    self.context.drop(&var);
                }
                Ok(())
                // Err(Diagnostic::error()
                //     .with_message("Cant check against quantification yet"))
            }
            // Subtyping
            (_, _) => {
                let a = self.synth(expr)?;
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
            // <:Unit
            (Type::Literal(a), Type::Literal(b)) => {
                debug!("<:Unit");
                if a == b {
                    Ok(())
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "Type '{}' is not assignable to type '{}'",
                        a, b
                    )))
                }
            }
            // <:Var
            (Type::Variable(alpha), Type::Variable(beta)) => {
                debug!("<:Var");
                if alpha == beta {
                    Ok(())
                } else {
                    panic!("Cant subtype type variables")
                }
            }
            // <:Exvar
            (Type::Existential(alpha), Type::Existential(beta))
                if alpha == beta =>
            {
                // TODO check well formed
                Ok(())
            }
            // <:->
            (
                Type::Function {
                    parameters: a1,
                    out: a2,
                    ..
                },
                Type::Function {
                    parameters: b1,
                    out: b2,
                    ..
                },
            ) => {
                debug!("<:->");
                for (a, b) in a1.iter().zip(b1) {
                    let a = self.apply_context(a.ty)?;
                    let b = self.apply_context(b.ty)?;
                    self.subtype(a, b)?;
                }
                let a2 = self.apply_context(*a2)?;
                let b2 = self.apply_context(*b2)?;
                self.subtype(a2, b2)
            }
            // >:forallL
            (Type::Quantification(alphas, a), _) => {
                debug!("<:\u{2200}L");

                // This is where we need to drop after we're done!
                // TODO revert these changes
                let mut substituted_a = *a;
                let start_marker =
                    Element::new_marker(self.fresh_existential());
                self.context.add(start_marker.clone());
                for alpha in alphas {
                    let r1 = self.fresh_existential();
                    self.context.add(Element::new_marker(r1));
                    self.context.add(Element::new_existential(r1));
                    substituted_a = self.substitution(
                        alpha,
                        *a,
                        Type::Existential(r1).into(),
                    )?;
                }
                self.subtype(substituted_a, b)?;
                // self.context.drop(&start_marker);
                Ok(())
                // Err(Diagnostic::error()
                //     .with_message("Cant subtype left quantification yet"))
            }
            // >:forallR
            (_, Type::Quantification(_alpha, _a)) => {
                debug!("<:\u{2200}R");
                Err(Diagnostic::error()
                    .with_message("Cant subtype right quantification yet"))
            }
            // <:InstantiateL
            (Type::Existential(alpha), _) => {
                if occurs_in(alpha, b) {
                    // This is a circular type, we can't figure it out
                    Err(Diagnostic::error().with_message(
                        "We found a circular type that we couldn't infer",
                    ))
                } else {
                    self.instantiate_l(*alpha, b)
                }
            }
            // <:InstantiateR
            (_, Type::Existential(alpha)) => {
                if occurs_in(alpha, a) {
                    // This is a circular type, we can't figure it out
                    Err(Diagnostic::error().with_message(
                        "We found a circular type that we couldn't infer",
                    ))
                } else {
                    self.instantiate_r(*alpha, a)
                }
            }

            // Error cases
            // Type variables subtyped with anything but another type variable
            (_, Type::Variable(_)) | (Type::Variable(_), _) => {
                let message = format!("Cannot subtype a known value with a type variable. This variable could be instantiated to an arbitrary type that does not match");
                Err(Diagnostic::error().with_message(message))
            }
            (_, Type::Function { .. }) | (Type::Function { .. }, _) => {
                Err(Diagnostic::error()
                    .with_message("Cant subtype with functions"))
            }
            (_, _) => Err(Diagnostic::error().with_message("Cant subtype")),
        }
    }

    fn instantiate_l(
        &mut self,
        alpha: Existential,
        b: InternType,
    ) -> Result<()> {
        debug!("<:InstantiateL - {:?} with {:?}", alpha, b);
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
        debug!("<:InstantiateR - {:?} with {:?}", alpha, a);
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

    /// I don't know if this is sound within the type system, but this allows us to
    /// perform explicit type applications for type aliases that instantiate type schemes
    /// with monotypes.
    fn type_application(
        &mut self,
        var: Variable,
        in_ty: InternType,
        with_ty: InternType,
    ) -> Result<InternType> {
        match &*in_ty {
            Type::Variable(var2) => {
                if &var == var2 {
                    Ok(with_ty)
                } else {
                    Ok(in_ty)
                }
            }
            Type::Function {
                parameters, out, ..
            } => {
                let mut s1 = vec![];
                for param in parameters {
                    let ty = self.type_application(var, param.ty, with_ty)?;
                    s1.push(ty::Parameter {
                        ty,
                        name: param.name.clone(),
                    })
                }
                Ok(Type::new_function(
                    s1,
                    self.type_application(var, *out, with_ty)?,
                ))
                // let mut t1 = vec![];
            }
            Type::Quantification(vars, ty) => {
                if vars.contains(&var) {
                    let mut vars = vars.clone();
                    let index = vars.iter().position(|e| e == &var).unwrap();
                    vars.remove(index);

                    let ty = self.type_application(var, *ty, with_ty)?;

                    if vars.is_empty() {
                        Ok(ty)
                    } else {
                        Ok(Type::new_quantification(vars, ty).into())
                    }
                } else {
                    Ok(in_ty)
                }
            }
            Type::Pair(a, b) => Ok(Type::Pair(
                self.type_application(var, *a, with_ty)?,
                self.type_application(var, *b, with_ty)?,
            )
            .into()),
            _ => Ok(in_ty),
        }
    }

    fn apply_context(&mut self, ty: InternType) -> Result<InternType> {
        debug!("apply_context {:?}", ty);
        match &*ty {
            Type::Existential(alpha) => {
                if let Some(tau) = self.context.get_solved(*alpha) {
                    debug!("solved existential {:?} as {:?}", alpha, tau);
                    self.apply_context(tau)
                } else {
                    Ok(ty)
                }
            }
            Type::Function {
                parameters,
                out,
                effect,
            } => {
                // panic!("exit function apply_context");
                let mut mapped_parameters = vec![];
                for param in parameters {
                    mapped_parameters.push(ty::Parameter {
                        ty: self.apply_context(param.ty)?,
                        name: param.name.clone(),
                    });
                }
                let output = self.apply_context(*out)?;
                Ok(Type::new_function_with_effect(
                    mapped_parameters,
                    output,
                    effect.clone(),
                ))
            }
            _ => Ok(ty),
        }
    }

    fn synthesize_application(
        &mut self,
        ty: InternType,
        args: Vec<&hir::Argument>,
    ) -> Result<InternType> {
        // Application is the primary point where new effects are introduced.

        // TODO is this right?

        let ty = self.apply_context(ty)?;

        match &*ty {
            Type::Adt { .. } => Err(Diagnostic::error()
                .with_message("Cant call an enum as a function")),
            Type::Function {
                parameters,
                out,
                effect,
            } => {
                if parameters.len() != args.len() {
                    return Err(Diagnostic::error()
                        .with_message("Wrong number of arguments provided"));
                }
                // TODO error handling for too many/few arguments
                assert_eq!(parameters.len(), args.len());

                // Checking the first argument will tell us if we're using named or
                // positional arguments, as the parser ensures the two aren't mixed.
                let is_named_arguments =
                    args.first().map(|arg| arg.name.is_some()).unwrap_or(false);

                if is_named_arguments {
                    use std::collections::HashMap;
                    let mut parameter_map: HashMap<
                        syntax::symbol::Symbol,
                        InternType,
                    > = HashMap::new();

                    for param in parameters {
                        let name = param.name.clone().expect("You are passing named arguments to a function type with no argument names. This is a compiler bug.");
                        parameter_map.insert(name, param.ty);
                    }

                    for argument in args {
                        let ident = argument.name.clone().unwrap().symbol;
                        match parameter_map.remove(&ident) {
                            Some(ty) => {
                                self.checks_against(&argument.value, ty)?;
                            }
                            None => panic!("Oops"),
                        }
                    }

                    // Parameter map should be cleared now. If not that means we're missing parameters
                    if !parameter_map.is_empty() {
                        return Err(Diagnostic::error().with_message(
                            "Missing parameter for function call",
                        ));
                    }

                // Check named arguments
                } else {
                    for (param, hir::Argument { value, .. }) in
                        parameters.iter().zip(args)
                    {
                        self.checks_against(value, param.ty)?;
                    }
                }

                // Check the arguments against the input types

                // This is the primary place where we combine effects; we know the effect
                // of this function, all arguments have checked, so now we extend the current
                // tracked effect with the effect of this function.

                let tracked_effet_ty = self.tracked_effect_ty.as_mut().unwrap();
                tracked_effet_ty.extend(effect);

                // If everything checks, return the output type
                Ok(*out)
            }
            Type::Quantification(vs, ty) => {
                debug!("\u{2200}App");
                let alpha = self.fresh_existential();
                self.context.add(Element::new_existential(alpha));
                let mut substituted = *ty;
                for v in vs {
                    substituted = self.substitution(
                        v,
                        *ty,
                        Type::Existential(alpha).into(),
                    )?;
                }
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
                let beta_params: Vec<ty::Parameter> = beta
                    .iter()
                    .map(|alpha| Type::Existential(*alpha).into())
                    .zip(args.iter())
                    .map(|(ty, argument)| ty::Parameter {
                        ty,
                        // Use the argument name as the inferred parameter name
                        name: argument.name.clone().map(|ident| ident.symbol),
                    })
                    .collect();

                // Create an existential function type
                let fn_ty = Type::new_function(
                    beta_params.clone(),
                    Type::Existential(alpha).into(),
                );

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

                for (hir::Argument { value, .. }, ref param) in
                    args.iter().zip(beta_params)
                {
                    self.checks_against(value, param.ty)?;
                }
                Ok(Type::Existential(alpha).into())
            }
            Type::Literal(_)
            | Type::SolvableExistential(_, _)
            | Type::Unit
            | Type::Pair(_, _)
            | Type::Tuple(_)
            | Type::List(_)
            | Type::Variable(_)
            | Type::Component { .. } => todo!(),
        }
    }

    fn substitution(
        &self,
        alpha: &Variable,
        a: InternType,
        b: InternType,
    ) -> Result<InternType> {
        debug!("substitution, alpha: {:?}\na: {:#?}\nb: {:#?}", alpha, a, b);
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
            Type::Quantification(vars, ty) => {
                // TODO validate this is the right way to do it
                let mut substituted = *ty;
                for var in vars {
                    if var == alpha {
                        substituted =
                            Type::Quantification(vars.clone(), b).into();
                    } else {
                        substituted = Type::Quantification(
                            vars.clone(),
                            self.substitution(alpha, *ty, b)?,
                        )
                        .into()
                    }
                }
                return Ok(substituted);
            }
            Type::Function {
                parameters, out, ..
            } => {
                let mut s1 = vec![];
                for param in parameters {
                    let ty = self.substitution(alpha, param.ty, b)?;
                    s1.push(ty::Parameter {
                        ty,
                        name: param.name.clone(),
                    })
                }
                Ok(Type::new_function(s1, self.substitution(alpha, *out, b)?))
                // let mut t1 = vec![];
            }
            Type::Existential(_var) => {
                // In what case would we replace a type variable with an existential?
                Ok(a)
            }
            Type::Pair(t1, t2) => Ok(Type::Pair(
                self.substitution(alpha, *t1, b)?,
                self.substitution(alpha, *t2, b)?,
            )
            .into()),
            Type::Tuple(tys) => {
                let mut sub_tys = vec![];
                for ty in tys {
                    sub_tys.push(self.substitution(alpha, *ty, b)?);
                }
                Ok(Type::Tuple(sub_tys).into())
            }
            _ => unimplemented!(),
        }
    }

    fn hir_type_to_type(&mut self, hir_type: &hir::Type) -> Result<InternType> {
        debug!("hir_type_to_type {:#?}", hir_type);
        let ty = match hir_type {
            HIRType::Enum { enumdef, .. } => {
                self.context.get_annotation(&enumdef.unique_name).unwrap()
            }
            HIRType::Tuple(hir_tys, _) => {
                let mut tys = vec![];
                for ty in hir_tys {
                    let ty = self.hir_type_to_type(ty)?;
                    tys.push(ty);
                }
                Type::Tuple(tys).into()
            }
            HIRType::Number(_) => Type::Literal(LiteralType::Number).into(),
            HIRType::String(_) => Type::Literal(LiteralType::String).into(),
            HIRType::Bool(_) => Type::Literal(LiteralType::Bool).into(),
            HIRType::Reference {
                alias, arguments, ..
            } => {
                // Here we need to apply the argumenst to the type
                let hir::TypeAlias {
                    parameters,
                    unique_name,
                    ..
                } = &**alias;
                debug!("reference to {:?}, args {:#?}", unique_name, arguments);

                // Resolve interned type for the alias, which should already exist in context
                let ty = self.context.get_annotation(unique_name).unwrap();
                debug!("reference type: {:#?}", ty);

                return match (parameters, arguments) {
                    (Some(parameters), Some(arguments)) => {
                        if parameters.len() != arguments.len() {
                            let msg = format!("Expected {} arguments, but {} arguments were provided", parameters.len(), arguments.len());
                            let span = hir_type.span();
                            return Err(Diagnostic::error()
                                .with_message(msg)
                                .with_labels(vec![
                                    Label::primary(span),
                                    Label::secondary(alias.name.span),
                                ]));
                        }

                        let mut ty = ty;

                        for (hir::TVar { unique_name, name }, arg) in
                            parameters.iter().zip(arguments.iter())
                        {
                            debug!("substituting arguments, ty is {:#?}", ty);
                            // Find the type variable for the parameter
                            let var = self
                                .variable_map
                                .get(&unique_name)
                                .unwrap()
                                .clone();
                            let arg_ty = self.hir_type_to_type(arg)?;
                            ty = self.type_application(var, ty, arg_ty)?;
                        }
                        return Ok(ty);
                    }
                    (None, None) => {
                        Ok(ty)
                        // No arguments, good to go
                    }
                    (None, Some(args)) => {
                        // Arguments but no parameters!
                        let arg_count = args.len();

                        // Merge the spans of the reference and its arguments
                        let span = {
                            let mut span = hir_type.span();
                            for arg in args {
                                span = span.merge(arg.span())
                            }
                            span
                        };
                        let msg = format!("The type '{:?}' takes no arguments, but {} argument{} provided.", alias.name, arg_count, if arg_count > 1 {"s were"} else { " was"});
                        Err(Diagnostic::error().with_message(msg).with_labels(
                            vec![
                                Label::primary(span),
                                Label::secondary(alias.name.span),
                            ],
                        ))
                    }
                    (Some(params), None) => {
                        let param_count = params.len();
                        let span = hir_type.span();
                        let msg = format!(
                            "The type '{:?}' requires {} argument{}, but none were provided.",
                            alias.name,
                            param_count,
                            if param_count > 1  {"s"} else {""}
                        );
                        Err(Diagnostic::error().with_message(msg).with_labels(
                            vec![
                                Label::primary(span),
                                Label::secondary(alias.name.span),
                            ],
                        ))
                    }
                };
            }
            HIRType::Function { parameters, out } => {
                let out = self.hir_type_to_type(out)?;
                let mut ty_parameters = vec![];
                for param in parameters {
                    let ty = self.hir_type_to_type(&param.ty)?;
                    ty_parameters.push(ty::Parameter {
                        name: param.name.clone(),
                        ty,
                    })
                }
                // let parameters = self.hir_type_to_type(input)?;
                // let out = self.hir_type_to_type(output)?;
                // This is where type application is done.
                Type::new_function(ty_parameters, out)
            }
            HIRType::Var(_, unique_name) => {
                let var = self
                    .variable_map
                    .get(unique_name)
                    .expect("Expected variable");
                Type::Variable(*var).into()
            }
        };
        Ok(ty)
        // let ty = match &hir_type.kind {
        //     // Types that we can't resolve. These should be built-ins
        //     // that the HIR doesn't know about
        //     TypeKind::UnresolvedReference(typename) => {
        //         match typename.symbol.as_str() {
        //             "number" => Type::Literal(LiteralType::Number),
        //             "boolean" => Type::Literal(LiteralType::Boolean),
        //             "string" => Type::Literal(LiteralType::String),
        //             "Array" => {
        //                 let variable = self.fresh_variable();
        //                 Type::Quantification(
        //                     vec![variable],
        //                     Type::List(Type::Variable(variable).into()).into(),
        //                 )
        //             }
        //             _ => {
        //                 panic!("Cant resolve");
        //             }
        //         }
        //         .into()
        //     }
        //     TypeKind::Reference(typedef) => {
        //         let name = typedef.unique_name;
        //         self.context.get_annotation(&name).unwrap()
        //     }
        //     TypeKind::Tuple(hir_tys) => {
        //         let mut tys = vec![];
        //         for ty in hir_tys {
        //             tys.push(self.hir_type_to_type(ty)?);
        //         }
        //         Type::Tuple(tys).into()
        //     }
        //     TypeKind::Function(input, output) => {
        //         let input = match &input.kind {
        //             TypeKind::Tuple(hir_tys) => {
        //                 let mut parameters = vec![];
        //                 for ty in hir_tys {
        //                     parameters.push(self.hir_type_to_type(ty)?);
        //                 }
        //                 parameters
        //             }
        //             TypeKind::Reference(_)
        //             | TypeKind::Function(_, _)
        //             | TypeKind::UnresolvedReference(_)
        //             | TypeKind::TypeParameter(_) => {
        //                 let ty = self.hir_type_to_type(input)?;
        //                 vec![ty]
        //             }
        //         };
        //         let output = self.hir_type_to_type(output)?;
        //         let params: Vec<ty::Parameter> = input
        //             .iter()
        //             .map(|ty| ty::Parameter {
        //                 ty: *ty,
        //                 name: None,
        //             })
        //             .collect();
        //         Type::new_function(params, output)
        //         // Type::Function { parameters: input, out: output, effect: }.into()
        //     }
        //     TypeKind::TypeParameter(name) => {
        //         let variable = self.variable_map.get(name).unwrap();
        //         Type::Variable(*variable).into()
        //     }
        // };
        // Ok(ty)
    }
}

fn occurs_in(alpha: &Existential, ty: InternType) -> bool {
    debug!("occurs_in: {:?} - {:?}", alpha, ty);
    // false
    match &*ty {
        Type::Adt { .. } => {
            todo!("Occurs in ADT");
        }
        Type::Component { .. } => todo!("occurs_in for component"),
        Type::Unit => false,
        Type::Literal(_) => false,
        // TODO when is it possible for an existential to equal a type variable?
        Type::Variable(_) => false,
        Type::Function {
            parameters, out, ..
        } => {
            return occurs_in(alpha, *out)
                || parameters
                    .iter()
                    .any(|param| return occurs_in(alpha, param.ty))
        }
        Type::Existential(beta) => alpha == beta,
        Type::SolvableExistential(beta, _) => alpha == beta,
        Type::Pair(a, b) => occurs_in(alpha, *a) || occurs_in(alpha, *b),
        Type::List(ty) => occurs_in(alpha, *ty),
        Type::Quantification(_beta, ty) => {
            // TODO alpha == beta condition, but WHEN COULD THAT HAPPEN?
            occurs_in(alpha, *ty)
        }
        Type::Tuple(tys) => tys.iter().any(|ty| occurs_in(alpha, *ty)),
    }
}

/// Infers the type of a literal expression
fn infer_literal(lit: &Lit) -> InternType {
    let lit_ty = match lit.kind {
        LitKind::Bool(_) => LiteralType::Bool,
        LitKind::Str(_) => LiteralType::String,
        LitKind::Number(_) => LiteralType::Number,
    };
    Intern::new(Type::Literal(lit_ty))
}
