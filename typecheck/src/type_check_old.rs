use ::hir::*;
use data_structures::module_graph::ModuleGraph;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use source::filesystem::FileId;
use syntax::symbol::Symbol;

use std::collections::HashMap;

use ::hir::visit::{self, walk_block, walk_function, Visitor};

use ty::{Existential, LiteralType, Type};
use crate::type_context::{TypeContext, Element};

use std::sync::{Arc, Mutex};

macro_rules! return_symbol {
    () => {
        Symbol::intern("@return")
    };
}

macro_rules! number {
    () => {
        Type::Literal(LiteralType::Number)
    };
}

pub struct TyCtx {
    file: FileId,
    module_graph: Arc<Mutex<ModuleGraph<Module, Symbol>>>,
    // The number of existentials
    existentials: u16,
    // Ordered context of type elements
    context: TypeContext,
    // The return type for the current function
    current_return_ty: Option<Type>,
    // Function types
    // fn_types: HashMap<Symbol, Type>,
}

impl Visitor for TyCtx {
    // We type check a module one function at a time.
    fn visit_function(&mut self, func: &Function) -> Result<()> {
        // If there are generics, we need to type this function as Type::Quantification,
        // rather than Type::VariadicFunction. This is a hacky solution for now
        let mut quantification_symbols = vec![];

        // Check for generics first.
        if let Some(generics) = &func.generics {
            for ident in &generics.params {
                quantification_symbols.push(ident.name.clone());
                let elem = Element::new_typed_variable(
                    ident.name.clone(),
                    Type::Variable(ident.name.clone()),
                );
                self.context.add(elem);
            }
        }

        // Iterate through the parameters and populate the type context. If the
        // parameters are annoted we consider the type solved, otherwise use
        // an existential.
        for param in &func.params {
            let name: Symbol = param.local.clone().into();
            match param.ty {
                // This is just a placeholder existential type that the HIR uses
                // since it can't create existentials. Maybe it should be able to?
                Type::UnknownExistential_DO_NOT_USE => {
                    let alpha = self.fresh_existential();
                    let existential = Element::new_existential(alpha);
                    let elem = Element::new_typed_variable(name, Type::Existential(alpha));
                    self.context.add(existential);
                    self.context.add(elem);
                }
                _ => {
                    let elem = Element::new_typed_variable(name, param.ty.clone());
                    self.context.add(elem);
                }
            }
        }

        // TODO dedupe with param logic
        let ty = match func.ty {
            // This is just a placeholder existential type that the HIR uses
            // since it can't create existentials. Maybe it should be able to?
            Type::UnknownExistential_DO_NOT_USE => {
                let alpha = self.fresh_existential();
                let existential = Element::new_existential(alpha);
                let elem = Element::new_typed_variable(
                    // Use a special symbol for the return type, which can't be represented in
                    // the source text.
                    return_symbol!(),
                    Type::Existential(alpha),
                );
                self.context.add(existential);
                self.context.add(elem.clone());
                Type::Existential(alpha)
            }
            _ => {
                let elem = Element::new_typed_variable(return_symbol!(), func.ty.clone());
                self.context.add(elem.clone());
                func.ty.clone()
            }
        };

        self.current_return_ty = Some(ty);

        // Now we need to check the return type of the function. If it's annotated, we add
        // that to the type context and just check return statements against it. If it's *not*
        // annotated, we need to create an existential for it so it can be subtype'd and refined.
        // TODO
        walk_function(self, func)?;

        // The types for the parameters and return value should be solved by now. Now we can create
        // the function type and add it to the context list.
        let output_ty = self.apply_context(self.current_return_ty.clone().unwrap())?;

        let mut input_ty = vec![];

        // Get the solved types for the parameters
        for param in &func.params {
            let name = param.local.clone().into();
            let ty = self.apply_context(self.context.get_annotation(&name)?.clone())?;
            input_ty.push(ty);
        }

        let mut ty = Type::VariadicFunction(input_ty, output_ty.into());

        if !quantification_symbols.is_empty() {
            ty = Type::VariadicQuantification(quantification_symbols, ty.into())
        }

        let ty = self.apply_context(ty)?;


        self.elements
            .push(Element::new_typed_variable(func.name.name.clone(), ty));

        Ok(())
        // After we walk the function, we should now know its type. We need to add this to some
        // kind of registry so we can easily type check function application.
        // TODO
    }

    fn visit_statement(&mut self, statement: &hir::Statement) -> Result<()> {
        match &statement.kind {
            StatementKind::Local(local) => {
                let Local { name, init, .. } = &**local;
                if let Some(expr) = init {
                    let synth_ty = self.synth(&*expr)?;
                    let sym: Symbol = name.clone().into();
                    // TODO check if there is an annotation on the local and check the synth type against it
                    let elem = Element::new_typed_variable(name.clone().into(), synth_ty);
                    self.context.add(elem)
                }
            }
            // For `if` statements we check the condition expression evaluates to `boolean`
            // and then just walk the block(s) and check all of the statements.
            StatementKind::If(IfExpr {
                span,
                condition,
                block,
                alt,
            }) => {
                // Check the condition is a boolean
                self.checks_against(condition, &Type::Literal(LiteralType::Boolean))?;
                // Walk the rest of the block
                walk_block(self, block)?;
            }
            StatementKind::Expr(expr_stmt) => match expr_stmt.kind {
                ExprKind::Assign(_, _, _) => {
                    self.synth(expr_stmt)?;
                }
                _ => {}
            },
            // For return statements we need to check the type against either:
            //   * the annotated return type
            //   * the existential type that we created for the return type
            StatementKind::Return(expr) => {
                let expected_return_ty = self.current_return_ty.as_ref().unwrap().clone();
                self.checks_against(expr, &expected_return_ty)?;
                let ty = self.apply_context(expected_return_ty)?;
                // let return_ty = match expected_return_ty {
                //     Type::Existential(alpha) => {
                //         self.get_solved(&alpha).expect("Unsolved existential")
                //     }
                //     _ => expected_return_ty,
                // };
            }
            _ => {}
        }
        Ok(())
    }
}

impl TyCtx {
    pub fn new(file: FileId, module_graph: Arc<Mutex<ModuleGraph<Module, Symbol>>>) -> Self {
        TyCtx {
            file,
            module_graph,
            existentials: 0,
            elements: vec![],
            context: TypeContext::default(),
            current_return_ty: None,
        }
    }

    /// Start checking the program from the root module, following imports.
    /// This will look for the `main` function in the module to get started.
    pub fn check_from_root(&mut self, root_module: &Module) -> Result<()> {
        self.visit_module(root_module)?;
        Ok(())
    }

    /// Synthesis a type from an expression
    fn synth(&mut self, expr: &Expr) -> Result<Type> {
        let t = self.snythesizes_to(&expr)?;
        let t = self.apply_context(t)?;
        Ok(t)
    }

    fn checks_against(&mut self, expr: &Expr, ty: &Type) -> Result<()> {
        // Only handling the hard case right now: subtyping
        let a = self.snythesizes_to(expr)?;
        let a = self.apply_context(a)?;
        let b = self.apply_context(ty.clone())?;
        match (&expr.kind, ty) {
            (ExprKind::Lit(_), Type::Literal(lit_ty)) => {
                let ty = self.synth(expr)?;
                if ty == Type::Literal(lit_ty.clone()) {
                    Ok(())
                } else {
                    Err(Diagnostic::error()
                        .with_message(format!("{:?} and {:?} are not equal", ty, lit_ty))
                        .with_labels(vec![Label::primary(expr.span).with_message(format!(
                            "Inferred a type of {:?} but wanted an {:?}",
                            ty, lit_ty
                        ))]))
                }
                // TODO check this
            }
            (_, _) => {
                // TODO apply context
                let _ty = self
                    .subtype(&a, &b)
                    // Add a label for the expression if it fails to check
                    .map_err(|err| err.with_labels(vec![Label::primary(expr.span)]))?;
                Ok(())
            }
        }
    }

    fn subtype(&mut self, a: &Type, b: &Type) -> Result<()> {
        match (a, b) {
            (_, Type::Pair(t1, t2)) => {
                self.subtype(a, t1)?;
                self.subtype(a, t2)
            }
            (Type::Literal(a), Type::Literal(b)) => {
                if a == b {
                    Ok(())
                } else {
                    // Bad error messaging, isn't clear which type is expected. Need to figure
                    // out how to handle that TODO.
                    Err(Diagnostic::error().with_message(format!(
                        "Failed to subtype: literal types {:?} and {:?} are not compatible",
                        a, b
                    )))
                }
            }
            (Type::Variable(alpha1), Type::Variable(alpha2)) => {
                if self.is_well_formed(a) && a == b {
                    Ok(())
                } else {
                    panic!("variable subtype")
                }
            }
            // <:InstantiateL
            (Type::Existential(alpha), _) => self.instantate_l(alpha, b),
            // <:InstantiateR
            (_, Type::Existential(alpha)) => self.instantate_r(a, alpha),
            _ => Err(Diagnostic::error().with_message("Cant subtype these types yet")),
        }
    }

    fn instantate_r(&mut self, a: &Type, alpha: &Existential) -> Result<()> {
        let (left, right) = self.split_at(Element::new_existential(alpha.clone()));

        if a.is_monotype() && self.is_well_formed(a) {
            self.context.insert_in_place(
                &Element::new_existential(alpha.clone()),
                vec![Element::Solved(alpha.clone(), a.clone())],
            )?;
            return Ok(());
        }

        Err(Diagnostic::error().with_message("<:InstantiateR not implemented yet"))
    }

    fn instantate_l(&mut self, alpha: &Existential, b: &Type) -> Result<()> {
        // First we need to split the context into left and right sets at
        // the index for the existential element
        let split_element = Element::new_existential(alpha.clone());
        let (left, right) = self.split_at(split_element);
        // InstLSolve

        // TODO is_well_formed on left?
        if b.is_monotype() && self.is_well_formed(&b) {
            // TODO improve this. Solving an exisential can be modeled as mutating the
            // element at the index
            self.context.insert_in_place(
                &Element::new_existential(alpha.clone()),
                vec![Element::Solved(alpha.clone(), b.clone())],
            )?;
            return Ok(());
            // TODO insert solved in place
        }
        Err(Diagnostic::error().with_message("<:InstantiateL not implemented yet"))
    }


    /// Splits the context into two segments: before an existential and after
    fn split_at(&self, element: Element) -> (Vec<Element>, Vec<Element>) {
        if let Some(index) = self.elements.iter().position(|ele| ele == &element) {
            let (lhs, rhs) = self.elements.split_at(index);
            return (lhs.to_vec(), rhs.to_vec());
        }
        panic!();
    }

    /// Version of `application_synthesizes_to that supports multiple arguments
    fn _variadic_application_synthesizes_to(
        &mut self,
        ty: &Type,
        exprs: Vec<Expr>,
    ) -> Result<Type> {
        match ty {
            Type::VariadicFunction(inputs, output) => {
                if exprs.len() != inputs.len() {
                    panic!("Not the right number of arguments");
                }
                let mut i = 0;
                while i < exprs.len() {
                    let expr = &exprs[i];
                    let ty = &inputs[i];
                    self.checks_against(expr, &ty)?;
                    i += 1;
                }
                // If all the arguments check, return the output type
                Ok(*output.clone())
            }
            Type::Quantification(alpha, a) => {
                // New existential for this quantification
                let alpha1 = self.fresh_existential();
                // Add the existential to the type context
                self.elements
                    .push(Element::new_existential(alpha1.clone()));
                // Apply the existential
                let substituted_a = self.substitution(a, &alpha, &Type::Existential(alpha1))?;
                self._variadic_application_synthesizes_to(&substituted_a, exprs)
            }

            Type::VariadicQuantification(alphas, a) => {

                let mut sub_a = *a.clone();

                for alpha in alphas {
                    // New existential for each quantification
                    let alpha1 = self.fresh_existential();
                    // Add the new existential tht context
                    self.context.add(Element::new_existential(alpha1));
                    // Replace the type variable with the new existential in the quantified
                    sub_a = self.substitution(&sub_a, alpha, &Type::Existential(alpha1))?;
                }


                self._variadic_application_synthesizes_to(&sub_a, exprs)
            }

            Type::Existential(alpha) => {
                let alpha1 = self.fresh_existential();
                let alpha2 = self.fresh_existential();
                self.context.insert_in_place(
                    &Element::new_existential(alpha.clone()),
                    vec![
                        Element::new_existential(alpha2.clone()),
                        Element::new_existential(alpha1.clone()),
                        Element::Solved(
                            alpha.clone(),
                            Type::VariadicFunction(
                                vec![Type::Existential(alpha1).into()],
                                Type::Existential(alpha2).into(),
                            ),
                        ),
                    ],
                )?;
                // TODO more than one argument?
                let expr = &exprs[0];
                self.checks_against(&expr, &Type::Existential(alpha1.clone()))?;
                Ok(Type::Existential(alpha2.clone()))
            }

            Type::Literal(_) => {
                panic!("Cant apply to this type");
            }

            Type::SolvableExistential(_, _) => {
                panic!("Cant apply to this type");
            }
            Type::UnknownExistential_DO_NOT_USE => {
                panic!("Cant apply to this type");
            }
            Type::Unit => {
                panic!("Cant apply to this type");
            }
            Type::Function(_, _) => {
                panic!("Cant apply to this type");
            }
            Type::Pair(_, _) => {
                panic!("Cant apply to this type");
            }
            Type::List(_) => {
                panic!("Cant apply to this type");
            }

            Type::Variable(_) => {
                panic!("Cant apply to this type");
            }
            Type::Enum => {
                panic!("Cant apply to this type");
            }
            Type::Record => {
                panic!("Cant apply to this type");
            }
        }
    }

    fn application_snythesizes_to(&mut self, ty: &Type, expr: &Expr) -> Result<Type> {
        match ty {
            Type::Function(input, output) => {
                // Check the expression that we're applying the function to against
                // the type of the funciton's input.
                //. We need to handle checking *multiple* expressions against a function type.
                self.checks_against(expr, input)?;
                // If it checks, we know to use the return type
                Ok(*output.clone())
            }
            Type::Quantification(alpha, a) => {
                // New existential for this quantification
                let alpha1 = self.fresh_existential();
                // Add the existential to the type context
                self.elements
                    .push(Element::new_existential(alpha1.clone()));
                // Apply the existential
                let substituted_a = self.substitution(a, &alpha, &Type::Existential(alpha1))?;
                self.application_snythesizes_to(&substituted_a, expr)
            }

            _ => {
                Err(Diagnostic::error().with_message("Failed to apply"))
                // ...
            }
        }
    }

    fn substitution(&mut self, a: &Type, alpha: &Symbol, b: &Type) -> Result<Type> {
        Ok(match a {
            Type::Literal(_) => a.clone(),
            Type::Variable(var) => {
                if var == alpha {
                    b.clone()
                } else {
                    a.clone()
                }
            }
            Type::Quantification(var, ty) => {
                if var == alpha {
                    Type::Quantification(var.clone(), b.clone().into())
                } else {
                    Type::Quantification(var.clone(), self.substitution(ty, alpha, b)?.into())
                }
            }
            Type::Existential(existential) => {
                // TODO I guess? How do we subtitute existentials here? The `alpha` is a symbol
                // which is different than our `Existential` type.
                panic!("Cant substitute existentials");
                // a.clone()
            }
            Type::Function(t1, t2) => Type::Function(
                self.substitution(t1, alpha, b)?.into(),
                self.substitution(t2, alpha, b)?.into(),
            ),
            // TODO we probably need to create multiple existentials for each substitution?
            Type::VariadicFunction(t1, t2) => {
                let mut s1 = vec![];
                for ty in t1 {
                    s1.push(self.substitution(ty, alpha, b)?);
                }
                Type::VariadicFunction(s1, self.substitution(t2, alpha, b)?.into())
            }
            Type::Pair(t1, t2) => Type::Pair(
                self.substitution(t1, alpha, b)?.into(),
                self.substitution(t2, alpha, b)?.into(),
            ),
            _ => unreachable!(),
        })
    }

    fn snythesizes_to(&mut self, expr: &Expr) -> Result<Type> {
        match &expr.kind {
            // Literal types
            ExprKind::Lit(lit) => Ok(self.literal_synthesizes_to(&lit)),
            // References
            ExprKind::Reference(ident, _binding) => {
                let t = self.context.get_annotation(&ident)?;
                Ok(t.clone())
            }
            ExprKind::Call(call, args) => {
                match call {
                    hir::Binding::Function(f) => {
                        let f = f.lock().unwrap();
                        let name = &f.name;
                        let ty = self.context.get_annotation(name)?;
                        let ty =
                            self._variadic_application_synthesizes_to(&ty.clone(), args.clone())?;
                        self.apply_context(ty)
                    }
                    hir::Binding::Argument(param) => {
                        let local = param.local.clone();
                        let name = local.into();
                        let ty = self.context.get_annotation(&name)?;
                        let ty = self.apply_context(ty.clone())?;
                        let ty = self._variadic_application_synthesizes_to(&ty, args.clone())?;
                        self.apply_context(ty)
                    }
                    _ => {
                        panic!("Cant call this as a function");
                        //
                    }
                }
            }
            // Assignments
            ExprKind::Assign(op, local, value) => {
                let ident: hir::Ident = local.name.clone().into();
                let ty = self.context.get_annotation(&ident)?.clone();
                // let t2 = self.synth(&*value)?;
                self.checks_against(value, &ty)?;
                Ok(ty.clone())
            }
            // For the type system, unary expressions are just function applications with the form
            // of `<A, B>(A) -> B`. Typically they are actually just `number -> number` as well.
            // Here we map the unary operator to the function type we've hardcoded for it.
            ExprKind::Unary(op, right) => {
                let a = match op {
                    // Currently all our operators map to the same funciton type. Using a match
                    // expression here in case that ever changes.
                    UnOp::Increment | UnOp::Negate | UnOp::Plus => Type::function(
                        Type::Literal(LiteralType::Number),
                        Type::Literal(LiteralType::Number),
                    ),
                    // TODO we currently use the negate unary operator as a test ground
                    // for single-argument polymorphic functions. It is essentially the identity
                    // function for now. Remove this special case once we figure it out.
                    UnOp::Minus => Type::Quantification(
                        Symbol::intern("@++"),
                        Type::function(
                            Type::Variable(Symbol::intern("@++").into()),
                            Type::Variable(Symbol::intern("@++").into()),
                        )
                        .into(),
                    ),
                };
                let ty = self.application_snythesizes_to(&a, right)?;
                self.apply_context(ty)
            }
            ExprKind::Binary(op, left, right) => {
                // We treat binary expressions as function application, as they are semantically
                // the same. For example, a + b is the same as add(a, b). We need to synthesis
                // a type for the function from the binary operator.

                let ty = match op {
                    // <T>(T, T) => bool
                    BinOp::DblEquals => {
                        let fn_ty = Type::Quantification(
                            Symbol::intern("@=="),
                            Type::VariadicFunction(
                                vec![
                                    Type::Variable(Symbol::intern("@==")).into(),
                                    Type::Variable(Symbol::intern("@==")).into(),
                                ],
                                Type::Literal(LiteralType::Boolean).into(),
                            )
                            .into(),
                        );
                        let exprs = vec![(**left).clone(), (**right).clone()];
                        let ty = self._variadic_application_synthesizes_to(&fn_ty, exprs)?;
                        return Ok(self.apply_context(ty)?);
                    }
                    BinOp::Or => {
                        let fn_ty = Type::Quantification(
                            Symbol::intern("T"),
                            Type::VariadicFunction(
                                vec![Type::Variable(Symbol::intern("T")), number!()],
                                Type::Variable(Symbol::intern("T")).into(),
                                // Type::Variable(Symbol::intern("T").into()),
                                // Type::Variable(Symbol::intern("@++").into()),
                            )
                            .into(),
                        );
                        let exprs = vec![(**left).clone(), (**right).clone()];
                        let ty = self._variadic_application_synthesizes_to(&fn_ty, exprs)?;
                        return Ok(self.apply_context(ty)?);
                    }
                    BinOp::Div | BinOp::Mul | BinOp::Mod => {
                        // We model this as div(left, right)
                        let fn_ty =
                            Type::VariadicFunction(vec![number!(), number!()], number!().into());
                        let exprs = vec![(**left).clone(), (**right).clone()];
                        let ty = self._variadic_application_synthesizes_to(&fn_ty, exprs)?;
                        return Ok(self.apply_context(ty)?);
                        // let expr = ExprKind::Tuple(vec![*left.clone(), *right.clone()]);
                        // self.application_snythesizes_to(&fn_ty, &expr)?
                    }
                    _ => panic!(),
                };

                //
                // let a = self.synth(left)?;
                // let b = self.synth(right)?;
                let message = "Cannot infer types for binary expressions yet.";
                Err(Diagnostic::error()
                    .with_message("Type failure")
                    .with_labels(vec![Label::primary(expr.span).with_message(message)]))
            }
            // Tuples
            // ExprKind::Tuple(exprs) => {
            //     let mut t = vec![];
            //     for expr in exprs {
            //         t.push(self.snythesizes_to(expr)?)
            //     }
            //     Ok(Type::Product(t))
            // }
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
                // let func = func.lock().unwrap();
                // Ok(Type::Variable)
                Err(Diagnostic::error().with_message("Cannot synthesis function types"))
            }
            _ => Err(Diagnostic::error().with_message("Type synthesis failed!")),
        }
    }

    fn apply_context(&mut self, ty: Type) -> Result<Type> {
        match ty {
            Type::Existential(alpha) => {
                // Check if we can solve this existential
                if let Some(tau) = self.context.get_solved(&alpha) {
                    self.apply_context(tau.clone())
                } else {
                    Ok(ty)
                }
            }
            Type::Function(a, b) => Ok(Type::Function(
                self.apply_context(*a)?.into(),
                self.apply_context(*b)?.into(),
            )),
            Type::VariadicFunction(t1, output) => {
                let mut t2 = vec![];
                for ty in t1 {
                    t2.push(self.apply_context(ty)?);
                }
                Ok(Type::VariadicFunction(
                    t2.into(),
                    self.apply_context(*output)?.into(),
                ))
            }
            // TODO other types
            _ => Ok(ty),
        }
    }

    /// Infer the type of a literal value.
    fn literal_synthesizes_to(&self, lit: &Lit) -> Type {
        let lit_ty = match lit.kind {
            LitKind::Bool(_) => LiteralType::Boolean,
            LitKind::Str(_) => LiteralType::String,
            LitKind::Number(_) => LiteralType::Number,
        };
        Type::Literal(lit_ty)
    }

    fn fresh_existential(&mut self) -> Existential {
        let e = Existential(self.existentials);
        self.existentials += 1;
        e
    }

    fn is_well_formed(&self, ty: &Type) -> bool {
        // TODO
        match ty {
            Type::Literal(_) => true,
            _ => true,
        }
    }
}
