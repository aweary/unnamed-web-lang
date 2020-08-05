use crate::type_context::{Element, TypeContext};
use ::common::unique_name::UniqueName;
use ::hir::{
    hir, BinOp, Binding, Expr, ExprKind, Lit, LitKind, Local, StatementKind,
    Type as HIRType, UnOp,
};

use ::hir::visit::{walk_block, walk_component, walk_function, Visitor};
use data_structures::HashMap;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use ty::effects::{EffectConstant, EffectType};
use ty::{boolean, number, Existential, LiteralType, Type, Variable};

use internment::Intern;
use log::{debug, info};

use std::sync::Arc;
use std::time::Instant;
use syntax::symbol::Symbol;

type InternType = Intern<Type>;

#[derive(Debug, Hash, PartialEq, Eq)]
struct EnumVariantKey(UniqueName, Symbol);

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
    /// Constructors for struct instances
    struct_constructors: HashMap<UniqueName, InternType>,
}

impl Visitor for TypeChecker {
    fn visit_constant(&mut self, constant: &hir::Constant) -> Result<()> {
        debug!("visit_constant");
        let hir::Constant {
            unique_name,
            name,
            value,
            ty,
            span,
        } = constant;

        let infer_ty = self.synth(value)?;
        let ty = if let Some(ty) = ty {
            let ty = self.hir_type_to_type(ty)?;
            self.subtype(ty, infer_ty)?;
            self.apply_context(infer_ty)?
        } else {
            infer_ty
        };

        match &*ty {
            Type::Function { .. } => {
                return Err(Diagnostic::error()
                    .with_message("Constants can't reference functions")
                    .with_labels(vec![Label::primary(name.span)]))
            }
            _ => {}
        }

        debug!("constant inferred as {:#?}", ty);

        let element = Element::new_typed_variable(*unique_name, ty);
        self.context.add(element);

        Ok(())
    }

    fn visit_statement(&mut self, statement: &hir::Statement) -> Result<()> {
        debug!("\n\nvisit_statement");
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
                let ty = self.synth(expr)?;
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

    /// This is where we generate the types for structs. Structs are instantiated
    /// using the same syntax as function calls, and we treat their types much the same.
    /// The struct type itself is a potentially quantified function type which itself returns
    /// a sealed record type.
    fn visit_struct(&mut self, struct_: &hir::Struct) -> Result<()> {
        let hir::Struct {
            fields,
            parameters,
            name,
            unique_name,
            span,
        } = struct_;
        debug!("visit_struct");
        // TODO dedupe this with visit_fn_def
        // If there are generics, we need to treat this as a quantification type
        let tvars = if let Some(tvars) = &parameters {
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

        let fields = fields
            .iter()
            .map(|hir::StructField { name, ty }| {
                let ty = self.hir_type_to_type(ty)?;
                let unique_name = UniqueName::new();
                Ok(ty::StructField { 
                    name: name.symbol.clone(),
                    unique_name,
                    ty
                })
            })
            .collect::<Result<Vec<ty::StructField>>>()?;

        let struct_ty = Type::Struct {
            name: name.symbol.clone(),
            unique_name: *unique_name,
            // TODO dedupe with visit_enum
            tvars: tvars.clone().map(|tvars| {
                tvars
                    .iter()
                    .map(|tvar| Type::Variable(*tvar).into())
                    .collect()
            }),
            fields: fields.clone(),
        };
        let struct_ty = Intern::new(struct_ty);

        let parameters = fields
            .iter()
            .map(|field| {
                let parameter = ty::Parameter {
                    ty: field.ty,
                    name: Some(field.name.clone()),
                };
                Ok(parameter)
            })
            .collect::<Result<Vec<ty::Parameter>>>()?;

        let struct_constructor =
            Type::new_function(parameters, struct_ty.into());

        let struct_constructor = if let Some(tvars) = tvars {
            Type::new_quantification(tvars, struct_constructor).into()
        } else {
            struct_constructor
        };

        self.struct_constructors
            .insert(*unique_name, struct_constructor);

        let element = Element::new_typed_variable(*unique_name, struct_ty);
        self.context.add(element);

        // let hir::Struct { name }
        Ok(())
    }

    fn visit_enum_def(&mut self, enum_def: &hir::EnumDef) -> Result<()> {
        let hir::EnumDef {
            variants,
            parameters,
            name,
            unique_name,
            ..
        } = enum_def;

        // If there are generics, we need to treat this as a quantification type
        let tvars = if let Some(tvars) = &parameters {
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

        let ty = Type::Enum {
            name: name.symbol.clone(),
            unique_name: *unique_name,
            tys: tvars.clone().map(|tvars| {
                tvars
                    .iter()
                    .map(|tvar| Type::Variable(*tvar).into())
                    .collect()
            }),
        };

        let enum_ty = Intern::new(ty);

        self.context
            .add(Element::new_typed_variable(*unique_name, enum_ty));

        // If there are no type parameters, things are simple.
        // Variants are just monomorphic functions.

        for hir::Variant {
            unique_name,
            fields,
            ..
        } in variants
        {
            let ty = if let Some(fields) = fields {
                let tys = fields
                    .iter()
                    .map(|ty| self.hir_type_to_type(ty))
                    .collect::<Result<Vec<InternType>>>()?;
                Type::Constructor(Some(tys), enum_ty)
            // If there are fields, create a function type
            // where the input type is the field types
            // let ty = Type::new_function()
            } else {
                Type::Constructor(None, enum_ty)
                // If there are no fields, create a function type
                // where the input type is Unit
            };

            let ty = if let Some(tvars) = &tvars {
                Type::new_quantification(tvars.clone(), ty.into())
            } else {
                ty.into()
            };

            self.context
                .add(Element::new_typed_variable(*unique_name, ty.into()));
        }

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

        let effect_ty = EffectType::default();

        // Track it so we can refine it / check against it
        self.tracked_return_ty = Some(return_ty);
        self.tracked_effect_ty = Some(effect_ty);

        walk_component(self, component)?;

        Ok(())
    }

    fn visit_function(&mut self, function: &hir::Function) -> Result<()> {
        debug!("visit_function: {:?}", function.name);
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

        // Check for implicit return
        if let Some(stmt) = function.body.statements.last() {
            if let StatementKind::Expr(expr) = &stmt.kind {
                let ty = self.synth(&expr)?;
                self.subtype(ty, return_ty)?;
            }
        }

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
            struct_constructors: HashMap::default(),
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
                        let return_ty = self.apply_context(return_ty)?;
                        // Check for implicit return
                        if let Some(stmt) = block.statements.last() {
                            if let StatementKind::Expr(expr) = &stmt.kind {
                                let ty = self.synth(&expr)?;
                                self.subtype(ty, return_ty)?;
                            }
                        }
                        self.tracked_return_ty = cached_tracked_return_ty;
                        return_ty
                    }
                    hir::LambdaBody::Expr(expr) => self.synth(expr)?,
                };

                let input_ty = self.synth_fn_params(&params)?;
                let ty = Type::new_function(input_ty, return_ty);
                debug!("Lambda type: {:#?}", ty);
                Ok(ty)
            }
            ExprKind::Reference(ident, binding) => {
                debug!("synthesizing a reference!");
                match binding {
                    Binding::Local(local) | Binding::State(local) => {
                        let name = local.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        let ty = self.apply_context(ty)?;
                        debug!("local is {:#?}", ty);
                        Ok(ty)
                    }
                    Binding::Constant(constant) => {
                        let hir::Constant { unique_name, .. } = &**constant;
                        let ty =
                            self.context.get_annotation(unique_name).unwrap();
                        let ty = self.apply_context(ty)?;
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
                    Binding::Enum(enumdef) => {
                        // let name = enumdef.unique_name;
                        // let ty = self.context.get_annotation(&name).unwrap();
                        // Ok(ty);
                        let msg = format!(
                            "Expected a value, found enum '{:?}'",
                            enumdef.name
                        );
                        Err(Diagnostic::error()
                            .with_message(msg)
                            .with_labels(vec![Label::primary(ident.span)]))
                    }
                    Binding::Struct(struct_) => {
                        let name = struct_.unique_name;
                        let ty = self.context.get_annotation(&name).unwrap();
                        Ok(ty)
                    }
                    // Binding::Import(import) =>
                    Binding::Component(_)
                    | Binding::Import(_)
                    | Binding::Type(_)
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
            ExprKind::Call(expr, args) => {
                // TODO mapping &Vec<Expr> to Vec<&Expr> is weird
                let args = args.iter().collect::<Vec<&hir::Argument>>();
                let ty = self.synth(&*expr)?;
                let ty = self.synthesize_application(ty, args)?;
                Ok(ty)
            }
            ExprKind::TrailingClosure(a, b) => {
                todo!("cant synth type for trailing closure expressions");
            }
            ExprKind::Array(exprs) => {
                if exprs.is_empty() {
                    // No items means we can't infer the type yet, use an existential
                    let alpha = self.fresh_existential();
                    self.context.add(Element::new_existential(alpha));
                    return Ok(
                        Type::List(Type::Existential(alpha).into()).into()
                    );
                }

                let mut tys = exprs
                    .iter()
                    .map(|expr| {
                        let ty = self.synth(expr)?;
                        // We want to find the most general type for inferring lists
                        // let ty = self.principal_ty(ty)?;
                        Ok(ty)
                    })
                    .collect::<Result<Vec<InternType>>>()?;

                // Deduplicate all the types. If all types are equal then we should end
                // with an array containing a single type.
                tys.dedup();

                let ty = match tys[..] {
                    [ty] => ty,
                    [a, b, ..] => {
                        return Err(Diagnostic::error()
                            .with_message("Arrays require a single type"))
                    }
                    [] => unreachable!("tys should be non-empty"),
                };
                Ok(Type::List(ty).into())
            }
            ExprKind::Block(block) => {
                // The value of a block expression is
                // the last statement in the block *if* it's an expression
                // statement. If it's not, the value is Unit
                self.visit_block(block)?;

                let ty = if let Some(stmt) = block.statements.last() {
                    if let StatementKind::Expr(expr) = &stmt.kind {
                        self.synth(&expr)?
                    } else {
                        Type::Unit.into()
                    }
                } else {
                    Type::Unit.into()
                };
                Ok(ty)
            }
            ExprKind::Object(_) => todo!("cannot type check Object"),
            ExprKind::Tuple(_) => todo!("cannot type check Tuple"),
            ExprKind::Unary(unop, expr) => {
                let ty = self.synth(&*expr)?;
                let fn_ty = self.unary_op_ty(unop);
                let argument = hir::Argument {
                    span: expr.span,
                    value: *expr.clone(),
                    name: None,
                };
                let synth_ty =
                    self.synthesize_application(fn_ty, vec![&argument])?;
                Ok(synth_ty)
            }
            ExprKind::EnumVariant(enumdef, member) => {
                let hir::EnumDef { variants, .. } = &**enumdef;
                let variant = variants
                    .iter()
                    .find(|variant| variant.ident.symbol == member.symbol)
                    .ok_or(
                        Diagnostic::error().with_message("Cannot find variant"),
                    )?;
                let ty =
                    self.context.get_annotation(&variant.unique_name).unwrap();

                // If this variant contains values, return the type of the variant
                // type constructor. If there are no values, treat this as if
                // the constructor was being called with no arguments.
                if variant.fields.is_some() {
                    Ok(ty)
                } else {
                    self.synthesize_application(ty, vec![])
                }
            }
            ExprKind::Field(expr, ident) => {
                debug!("FieldExpression");
                let value = self.synth(&*expr)?;
                debug!("ty: {:#?}", value);
                debug!("ident: {:#?}", ident);
                match &*value {
                    Type::Struct {
                        name,
                        unique_name,
                        tvars,
                        fields,
                    } => {
                        let field = fields
                            .iter()
                            .find(|field| field.name == ident.symbol);
                        if let Some(field) = field {
                            Ok(field.ty)
                        } else {
                            let msg = format!(
                                "Struct '{:?}' has no field '{:?}'",
                                name, ident,
                            );
                            Err(Diagnostic::error().with_message(msg))
                        }
                    }
                    _ => Ok(Type::Unit.into()),
                }
            }
            ExprKind::If(if_expr) => self.synthesize_if_expr(if_expr),
            ExprKind::Cond(_, _, _) => todo!("cannot type check Cond"),
            ExprKind::Assign(_, _, _) => todo!("cannot type check Assign"),
            ExprKind::StateUpdate(_, _, _) => {
                todo!("cannot type check StateUpdate")
            }
            ExprKind::OptionalMember(_, _) => {
                todo!("cannot type check OptionalMember")
            }
            ExprKind::For(_, _, _) => todo!("cannot type check For"),
            ExprKind::Index(_, _) => todo!("cannot type check Index"),
            ExprKind::Return(_) => todo!("cannot type check Return"),
            ExprKind::Template(_) => todo!("cannot type check Template"),
            ExprKind::Match(_, _) => todo!("cannot type check Match"),
            ExprKind::Func(_) => todo!("cannot type check Func"),
            ExprKind::Member(_, _) => todo!("cannot check member expressions"),
        }
    }

    fn synthesize_if_expr(
        &mut self,
        hir::IfExpression {
            condition,
            body,
            alternate,
            span,
        }: &hir::IfExpression,
    ) -> Result<InternType> {
        // Check that the condition expression is a boolean
        self.checks_against(&*condition, boolean!())?;

        // The type of the body
        let ty = self.synth(body)?;
        // The type of any alternates, or just the body type
        // if none exist.
        if let Some(alternate) = &alternate {
            match &**alternate {
                hir::Else::Value(expr) => self.checks_against(expr, ty)?,
                hir::Else::IfExpression(if_expr) => {
                    // Construct a new expression so we can use checks_against
                    let span = if_expr.span;
                    let expr = hir::Expr {
                        // TODO find a way to not clone?
                        kind: hir::ExprKind::If(if_expr.clone()),
                        span,
                    };
                    self.checks_against(&expr, ty)?
                }
            }
        }
        // self.subtype(ty, alt_ty)
        //     .map_err(|d| d.with_labels(vec![Label::primary(*span)]))?;
        self.apply_context(ty)
    }

    fn unary_op_ty(&mut self, op: &UnOp) -> InternType {
        match op {
            UnOp::Negate => Type::new_function(
                vec![ty::Parameter {
                    ty: boolean!(),
                    name: None,
                }],
                boolean!(),
            ),
            UnOp::Plus | UnOp::Minus | UnOp::Increment => Type::new_function(
                vec![ty::Parameter {
                    ty: number!(),
                    name: None,
                }],
                number!(),
            ),
        }
    }

    fn bin_op_ty(&mut self, op: &BinOp) -> InternType {
        match op {
            // Numeric operators
            BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Mod
            | BinOp::Add
            | BinOp::BinOr
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
            BinOp::GreaterThan | BinOp::LessThan => Type::new_function(
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
                boolean!(),
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
            | BinOp::Sum
            | BinOp::And
            | BinOp::Or
            | BinOp::Pipeline => unimplemented!(),
        }
    }

    fn checks_against(&mut self, expr: &Expr, ty: InternType) -> Result<()> {
        debug!("check_against - {}", ty);
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
            }
            // Subtyping
            (_, _) => {
                let a = self.synth(expr)?;
                let b = self.apply_context(ty)?;
                self.subtype(a, b).map_err(|err| {
                    let Diagnostic {
                        message,
                        mut labels,
                        ..
                    } = err;
                    labels.push(Label::primary(expr.span));
                    Diagnostic::error()
                        .with_message(message)
                        .with_labels(labels)
                })?;
                Ok(())
            }
        }
    }

    fn subtype(&mut self, a: InternType, b: InternType) -> Result<()> {
        debug!("subtype, a: {}, b: {}", a, b);
        match (&*a, &*b) {
            // <:Unit
            (Type::Unit, Type::Unit) => Ok(()),
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
            (Type::List(a), Type::List(b)) => self.subtype(*a, *b),
            // Enum subtying
            (
                Type::Enum {
                    tys: tys_a,
                    unique_name: unique_name_a,
                    ..
                },
                Type::Enum {
                    tys: tys_b,
                    unique_name: unique_name_b,
                    ..
                },
            ) => {
                if unique_name_a == unique_name_b {
                    match (tys_a, tys_b) {
                        (None, None) => Ok(()),
                        (Some(tys_a), Some(tys_b)) => {
                            for (a, b) in tys_a.iter().zip(tys_b) {
                                self.subtype(*a, *b)?;
                            }
                            Ok(())
                        }
                        _ => panic!("oops"),
                    }
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "Type '{}' is not assignable to type '{}'",
                        a, b
                    )))
                }
            }
            (
                Type::Struct {
                    tvars: tys_a,
                    unique_name: unique_name_a,
                    ..
                },
                Type::Struct {
                    tvars: tys_b,
                    unique_name: unique_name_b,
                    ..
                },
            ) => {
                if unique_name_a == unique_name_b {
                    match (tys_a, tys_b) {
                        (None, None) => Ok(()),
                        (Some(tys_a), Some(tys_b)) => {
                            for (a, b) in tys_a.iter().zip(tys_b) {
                                self.subtype(*a, *b)?;
                            }
                            Ok(())
                        }
                        _ => panic!("oops"),
                    }
                } else {
                    Err(Diagnostic::error().with_message(format!(
                        "Type '{}' is not assignable to type '{}'",
                        a, b
                    )))
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
            (_, _) => Err(Diagnostic::error().with_message(format!(
                "Type '{}' is not assignable to type '{}'",
                a, b
            ))),
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
            Type::Struct {
                fields,
                name,
                unique_name,
                tvars,
            } => {
                let fields = fields
                    .iter()
                    .map(|field| {
                        let ty =
                            self.type_application(var, field.ty, with_ty)?;
                        Ok(ty::StructField {
                            name: field.name.clone(),
                            unique_name: field.unique_name,
                            ty,
                        })
                    })
                    .collect::<Result<Vec<ty::StructField>>>()?;
                // TODO dedupe with enum
                let tvars = if let Some(tys) = tvars {
                    let tys = tys
                        .iter()
                        .map(|ty| self.type_application(var, *ty, with_ty))
                        .collect::<Result<Vec<InternType>>>()?;
                    Some(tys)
                } else {
                    None
                };
                Ok(Type::Struct {
                    name: name.clone(),
                    fields,
                    unique_name: *unique_name,
                    tvars,
                }
                .into())
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
            Type::Enum {
                tys,
                unique_name,
                name,
            } => {
                let tys = if let Some(tys) = tys {
                    let tys = tys
                        .iter()
                        .map(|ty| self.type_application(var, *ty, with_ty))
                        .collect::<Result<Vec<InternType>>>()?;
                    Some(tys)
                } else {
                    None
                };
                let ty = Type::Enum {
                    tys,
                    unique_name: *unique_name,
                    name: name.clone(),
                };
                Ok(ty.into())
            }
            _ => Ok(in_ty),
        }
    }

    fn apply_context(&mut self, ty: InternType) -> Result<InternType> {
        debug!("apply_context {}", ty);
        match &*ty {
            Type::Enum {
                tys,
                unique_name,
                name,
            } => {
                let tys = if let Some(tys) = tys {
                    Some(
                        tys.iter()
                            .map(|ty| self.apply_context(*ty))
                            .collect::<Result<Vec<InternType>>>()?,
                    )
                } else {
                    None
                };
                Ok(Type::Enum {
                    tys,
                    unique_name: *unique_name,
                    name: name.clone(),
                }
                .into())
            }
            Type::Struct {
                tvars,
                unique_name,
                name,
                fields,
            } => {
                let tvars = if let Some(tvars) = tvars {
                    Some(
                        tvars
                            .iter()
                            .map(|ty| self.apply_context(*ty))
                            .collect::<Result<Vec<InternType>>>()?,
                    )
                } else {
                    None
                };
                let fields = fields
                    .iter()
                    .map(
                        |ty::StructField {
                             name,
                             unique_name,
                             ty,
                         }| {
                            Ok(ty::StructField {
                                name: name.clone(),
                                unique_name: *unique_name,
                                ty: self.apply_context(*ty)?,
                            })
                        },
                    )
                    .collect::<Result<Vec<ty::StructField>>>()?;
                Ok(Type::Struct {
                    tvars,
                    unique_name: *unique_name,
                    name: name.clone(),
                    fields,
                }
                .into())
            }
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
                let parameters = parameters
                    .iter()
                    .map(|param| {
                        Ok(ty::Parameter {
                            ty: self.apply_context(param.ty)?,
                            name: param.name.clone(),
                        })
                    })
                    .collect::<Result<Vec<ty::Parameter>>>()?;
                let output = self.apply_context(*out)?;
                Ok(Type::new_function_with_effect(
                    parameters,
                    output,
                    effect.clone(),
                ))
            }
            Type::List(ty) => Ok(Type::List(self.apply_context(*ty)?).into()),
            _ => Ok(ty),
        }
    }

    fn synthesize_application(
        &mut self,
        ty: InternType,
        args: Vec<&hir::Argument>,
    ) -> Result<InternType> {
        debug!("synthesize_application");
        let ty = self.apply_context(ty)?;
        match &*ty {
            // TODO handle generic enums
            Type::Constructor(input_ty, output_ty) => {
                if let Some(input_tys) = input_ty {
                    for (ty, hir::Argument { value, .. }) in
                        input_tys.iter().zip(args)
                    {
                        self.checks_against(value, *ty)?;
                    }
                    Ok(*output_ty)
                } else {
                    // If there are no arguments, there's nothing to check.
                    Ok(*output_ty)
                }
            }
            Type::Enum { .. } => Err(Diagnostic::error()
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
                            None => {
                                // TODO find similar name
                                let msg =
                                    format!("No parameter named '{:?}'", ident);
                                return Err(Diagnostic::error()
                                    .with_message(msg)
                                    .with_labels(vec![Label::primary(
                                        argument.span,
                                    )]));
                            }
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

                let tracked_effet_ty = self.tracked_effect_ty.as_mut();
                // TODO handle this when we have functions without setting effects like +
                if let Some(f) = tracked_effet_ty {
                    f.extend(effect);
                }

                // If everything checks, return the output type
                Ok(*out)
            }
            Type::Quantification(vs, ty) => {
                debug!("\u{2200}App");
                let mut substituted = *ty;
                for v in vs {
                    let alpha = self.fresh_existential();
                    self.context.add(Element::new_existential(alpha));
                    substituted = self.substitution(
                        v,
                        substituted,
                        Type::Existential(alpha).into(),
                    )?;
                    // debug!("substited to: {:#?}", substituted);
                }
                debug!("quantified type substitued to: {:#?}", substituted);
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
            Type::Struct { unique_name, .. } => {
                // Find the type constructor for this struct
                let constructor =
                    *self.struct_constructors.get(unique_name).unwrap();
                self.synthesize_application(constructor, args)
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
            Type::Literal(_) | Type::Unit => Ok(a),
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
                debug!("substited quantified: {:#?}", substituted);
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
            Type::List(ty) => {
                let ty = self.substitution(alpha, *ty, b)?;
                Ok(Type::List(ty).into())
            }
            Type::Enum {
                tys,
                unique_name,
                name,
            } => {
                let tys = if let Some(tys) = tys {
                    Some(
                        tys.iter()
                            .map(|ty| self.substitution(alpha, *ty, b))
                            .collect::<Result<Vec<InternType>>>()?,
                    )
                } else {
                    None
                };
                debug!("enum tys: {:#?}", tys);
                Ok(Type::Enum {
                    tys,
                    unique_name: *unique_name,
                    name: name.clone(),
                }
                .into())
            }
            Type::Struct {
                tvars,
                fields,
                unique_name,
                name,
            } => {
                let tvars = if let Some(tvars) = tvars {
                    Some(
                        tvars
                            .iter()
                            .map(|ty| self.substitution(alpha, *ty, b))
                            .collect::<Result<Vec<InternType>>>()?,
                    )
                } else {
                    None
                };
                let fields = fields
                    .iter()
                    .map(|field| {
                        let ty = self.substitution(alpha, field.ty, b)?;
                        Ok(ty::StructField {
                            unique_name: field.unique_name,
                            name: field.name.clone(),
                            ty,
                        })
                    })
                    .collect::<Result<Vec<ty::StructField>>>()?;
                Ok(Type::Struct {
                    tvars,
                    fields,
                    unique_name: *unique_name,
                    name: name.clone(),
                }
                .into())
            }
            Type::Constructor(input, output) => {
                let input = if let Some(tys) = input {
                    Some(
                        tys.iter()
                            .map(|ty| self.substitution(alpha, *ty, b))
                            .collect::<Result<Vec<InternType>>>()?,
                    )
                } else {
                    None
                };
                let output = self.substitution(alpha, *output, b)?;
                Ok(Type::Constructor(input, output).into())
            }
            _ => unimplemented!(),
        }
    }

    fn apply_type_arguments(
        &mut self,
        ty: InternType,
        name: &syntax::symbol::Symbol,
        parameters: &Option<Vec<hir::TVar>>,
        arguments: &Option<Vec<hir::Type>>,
    ) -> Result<InternType> {
        return match (parameters, arguments) {
            (Some(parameters), Some(arguments)) => {
                if parameters.len() != arguments.len() {
                    let msg = format!(
                        "Expected {} arguments, but {} arguments were provided",
                        parameters.len(),
                        arguments.len()
                    );
                    return Err(Diagnostic::error().with_message(msg));
                }

                let mut ty = ty;

                for (hir::TVar { unique_name, name }, arg) in
                    parameters.iter().zip(arguments.iter())
                {
                    debug!("substituting arguments, ty is {:#?}", ty);
                    // Find the type variable for the parameter
                    let var =
                        self.variable_map.get(&unique_name).unwrap().clone();
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
                let msg = format!("The type '{:?}' takes no arguments, but {} argument{} provided.", name, arg_count, if arg_count > 1 {"s were"} else { " was"});
                Err(Diagnostic::error().with_message(msg))
            }
            (Some(params), None) => {
                let param_count = params.len();
                let msg = format!(
                            "The type '{:?}' requires {} argument{}, but none were provided.",
                            name,
                            param_count,
                            if param_count > 1  {"s"} else {""}
                        );
                Err(Diagnostic::error().with_message(msg))
            }
        };
    }

    fn hir_type_to_type(&mut self, hir_type: &hir::Type) -> Result<InternType> {
        debug!("hir_type_to_type {:#?}", hir_type);
        let ty = match hir_type {
            HIRType::List(ty, _) => {
                let ty = self.hir_type_to_type(&*ty)?;
                Type::List(ty).into()
            }
            HIRType::Struct {
                arguments,
                struct_,
                span,
            } => {
                let hir::Struct {
                    name,
                    unique_name,
                    fields,
                    span,
                    parameters,
                } = &**struct_;
                let ty = self.context.get_annotation(&unique_name).unwrap();
                self.apply_type_arguments(
                    ty,
                    &name.symbol,
                    parameters,
                    arguments,
                )?
            }
            HIRType::Enum {
                enumdef,
                span,
                arguments,
            } => {
                let hir::EnumDef {
                    unique_name,
                    parameters,
                    name,
                    variants,
                    span,
                } = &**enumdef;
                let ty = self.context.get_annotation(&unique_name).unwrap();
                self.apply_type_arguments(
                    ty,
                    &name.symbol,
                    parameters,
                    arguments,
                )
                .map_err(|err| {
                    err.with_labels(vec![Label::primary(hir_type.span())])
                })?
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
            HIRType::Unit(_) => Type::Unit.into(),
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

                return self.apply_type_arguments(
                    ty,
                    &alias.name.symbol,
                    parameters,
                    arguments,
                );
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
    }
}

fn occurs_in(alpha: &Existential, ty: InternType) -> bool {
    debug!("occurs_in: {:?} - {}", alpha, ty);
    // false
    match &*ty {
        Type::Component { .. } => {
            todo!("occurs_in for component")
        }
        Type::Constructor(input, output) => {
            occurs_in(alpha, *output) || if let Some(input) = input {
                input.iter().any(|ty| occurs_in(alpha, *ty))
            } else { false }
        }
        Type::Struct { fields, .. }  => {
            // TODO
            false
        }
        // TODO
        Type::Enum { tys, .. } => tys
            .as_ref()
            .map(|tys| {
                tys.iter().any(|ty| {
                    occurs_in(alpha, *ty)
                })
            })
            .unwrap_or(false),
        Type::Unit |
        Type::Literal(_) |
        // TODO when is it possible for an existential to equal a type variable?
        Type::Variable(_) => false,
        Type::Function {
            parameters,
            out,
            ..
        } => {
            return occurs_in(alpha, *out)
                || parameters.iter().any(
                    |param| {
                        return occurs_in(
                            alpha, param.ty,
                        );
                    },
                )
        }
        Type::Existential(beta) => alpha == beta,
        Type::SolvableExistential(beta, _) => {
            alpha == beta
        }
        Type::Pair(a, b) => {
            occurs_in(alpha, *a)
                || occurs_in(alpha, *b)
        }
        Type::List(ty) => occurs_in(alpha, *ty),
        Type::Quantification(_beta, ty) => {
            // TODO alpha == beta condition, but WHEN COULD THAT HAPPEN?
            occurs_in(alpha, *ty)
        }
        Type::Tuple(tys) => tys
            .iter()
            .any(|ty| occurs_in(alpha, *ty)),
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
