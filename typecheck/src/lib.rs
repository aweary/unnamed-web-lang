use ::hir::*;
use data_structures::module_graph::ModuleGraph;
use diagnostics::ParseResult as Result;
use source::diagnostics::{Diagnostic, Label};
use source::filesystem::FileId;
use syntax::symbol::Symbol;

use ::hir::visit::{self, walk_block, walk_function, Visitor};

use ty::{Existential, LiteralType, Type};

use std::sync::{Arc, Mutex};

macro_rules! return_symbol {
    () => {
        Symbol::intern("@return")
    };
}

macro_rules! function {
    ($input: expr, $output: expr) => {
        Type::Function($input.into(), $output.into())
    };
}

macro_rules! pair {
    ($a: expr, $b: expr) => {
        Type::Pair($a.into(), $b.into())
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
    // Ordered list of type elements
    elements: Vec<TyCtxElement>,
    // The return type for the current function
    current_return_ty: Option<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum TyCtxElement {
    Variable(Symbol),
    Existential(Existential),
    Solved(Existential, Type),
    Marker(String),
    TypedVariable(Symbol, Type),
}

impl Visitor for TyCtx {
    // We type check a module one function at a time.
    fn visit_function(&mut self, func: &Function) -> Result<()> {
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
                    let existential = TyCtxElement::Existential(alpha);
                    let elem = TyCtxElement::TypedVariable(name, Type::Existential(alpha));
                    self.elements.push(existential);
                    self.elements.push(elem);
                }
                _ => {
                    let elem = TyCtxElement::TypedVariable(name, param.ty.clone());
                    self.elements.push(elem);
                }
            }
        }

        // TODO dedupe with param logic
        let ty = match func.ty {
            // This is just a placeholder existential type that the HIR uses
            // since it can't create existentials. Maybe it should be able to?
            Type::UnknownExistential_DO_NOT_USE => {
                let alpha = self.fresh_existential();
                let existential = TyCtxElement::Existential(alpha);
                let elem = TyCtxElement::TypedVariable(
                    // Use a special symbol for the return type, which can't be represented in
                    // the source text.
                    return_symbol!(),
                    Type::Existential(alpha),
                );
                self.elements.push(existential);
                self.elements.push(elem.clone());
                Type::Existential(alpha)
            }
            _ => {
                let elem = TyCtxElement::TypedVariable(return_symbol!(), func.ty.clone());
                self.elements.push(elem.clone());
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
        let return_ty = self.current_return_ty.clone().unwrap();
        println!("We are returning a: {:#?}", self.apply_context(return_ty)?);
        println!("{:#?}", self.elements);

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
                    println!("Local {:?} {:?}", sym, synth_ty);
                    // TODO check if there is an annotation on the local and check the synth type against it
                    let elem = TyCtxElement::TypedVariable(name.clone().into(), synth_ty);
                    self.elements.push(elem)
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
            // For return statements we need to check the type against either:
            //   * the annotated return type
            //   * the existential type that we created for the return type
            StatementKind::Return(expr) => {
                let expected_return_ty = self.current_return_ty.as_ref().unwrap().clone();
                self.checks_against(expr, &expected_return_ty)?;
                let ty = self.apply_context(expected_return_ty)?;
                // println!("{:?}", expected_return_ty);
                // println!("{:#?}", self.elements);
                // let return_ty = match expected_return_ty {
                //     Type::Existential(alpha) => {
                //         self.get_solved(&alpha).expect("Unsolved existential")
                //     }
                //     _ => expected_return_ty,
                // };
                // println!("We're returning a value! {:?}", return_ty);
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
            current_return_ty: None,
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
                    Err(Diagnostic::error().with_message("Failed to check literal expression"))
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
        // println!("Subtyping: {:?} {:?}", a, b);
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
            // <:InstantiateL
            (Type::Existential(alpha), _) => self.instantate_l(alpha, b),
            // <:InstantiateR
            (_, Type::Existential(alpha)) => self.instantate_r(a, alpha),
            _ => Err(Diagnostic::error().with_message("Cant subtype these types yet")),
        }
    }

    fn instantate_r(&mut self, a: &Type, alpha: &Existential) -> Result<()> {
        // println!("instantiate_r");
        let (left, right) = self.split_at(TyCtxElement::Existential(alpha.clone()));

        if a.is_monotype() && self.is_well_formed(a) {
            self.insert_in_place(
                &TyCtxElement::Existential(alpha.clone()),
                vec![TyCtxElement::Solved(alpha.clone(), a.clone())],
            )?;
            return Ok(());
        }

        Err(Diagnostic::error().with_message("<:InstantiateR not implemented yet"))
    }

    fn instantate_l(&mut self, alpha: &Existential, b: &Type) -> Result<()> {
        println!("instantiate_l");
        // First we need to split the context into left and right sets at
        // the index for the existential element
        let split_element = TyCtxElement::Existential(alpha.clone());
        let (left, right) = self.split_at(split_element);
        // InstLSolve

        // TODO is_well_formed on left?
        if b.is_monotype() && self.is_well_formed(&b) {
            // TODO improve this. Solving an exisential can be modeled as mutating the
            // element at the index
            println!("Solving {:?} as {:?}", alpha, b);
            self.insert_in_place(
                &TyCtxElement::Existential(alpha.clone()),
                vec![TyCtxElement::Solved(alpha.clone(), b.clone())],
            )?;
            return Ok(());
            // TODO insert solved in place
        }
        Err(Diagnostic::error().with_message("<:InstantiateL not implemented yet"))
    }

    fn insert_in_place(
        &mut self,
        element: &TyCtxElement,
        inserts: Vec<TyCtxElement>,
    ) -> Result<()> {
        if let Some(index) = self.elements.iter().position(|elem| elem == element) {
            let mut eles = self.elements.clone();
            let _ = eles.splice(index..=index, inserts).count();
            self.elements = eles;
            Ok(())
        } else {
            Err(Diagnostic::error().with_message("Cant insert element"))
        }
    }

    /// Splits the context into two segments: before an existential and after
    fn split_at(&self, element: TyCtxElement) -> (Vec<TyCtxElement>, Vec<TyCtxElement>) {
        println!("Splitting at {:?}", element);
        if let Some(index) = self.elements.iter().position(|ele| ele == &element) {
            let (lhs, rhs) = self.elements.split_at(index);
            return (lhs.to_vec(), rhs.to_vec());
        }
        panic!();
    }

    fn application_snythesizes_to(&mut self, ty: &Type, expr: &Expr) -> Result<Type> {
        match ty {
            Type::Function(input, output) => {
                // Check the expression that we're applying the function to against
                // the type of the funciton's input
                self.checks_against(expr, input)?;
                // If it checks, we know to use the return type
                Ok(*output.clone())
            }
            Type::Quantification(alpha, a) => {
                // New existential for this quantification
                let alpha1 = self.fresh_existential();
                // Add the existential to the type context
                self.elements
                    .push(TyCtxElement::Existential(alpha1.clone()));
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
        println!("Substitution: {:?} {:?} {:?}", a, alpha, b);
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
                // TODO I guess?
                panic!()
            }
            Type::Function(t1, t2) => Type::Function(
                self.substitution(t1, alpha, b)?.into(),
                self.substitution(t2, alpha, b)?.into(),
            ),
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
                let t = self.get_annotation(&ident)?;
                Ok(t.clone())
            }
            // For the type system, unary expressions are just function applications with the form
            // of `<A, B>(A) -> B`. Typically they are actually just `number -> number` as well.
            // Here we map the unary operator to the function type we've hardcoded for it.
            ExprKind::Unary(op, right) => {
                println!("Unary");
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
                    BinOp::DblEquals => Type::Quantification(
                        Symbol::intern("@=="),
                        Type::function(
                            Type::Pair(
                                Type::Variable(Symbol::intern("@==")).into(),
                                Type::Variable(Symbol::intern("@==")).into(),
                            ),
                            Type::Literal(LiteralType::Boolean),
                        )
                        .into(),
                    ),
                    BinOp::Div => {
                        let fn_ty = function!(pair!(number!(), number!()), number!());
                        let expr = ExprKind::Tuple(vec![*left.clone(), *right.clone()]);
                        self.application_snythesizes_to(&fn_ty, &expr)?
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
                if let Some(tau) = self.get_solved(&alpha) {
                    self.apply_context(tau.clone())
                } else {
                    Ok(ty)
                }
            }
            // TODO other types
            _ => Ok(ty),
        }
    }

    fn get_solved(&self, alpha: &Existential) -> Option<Type> {
        for elem in &self.elements {
            if let TyCtxElement::Solved(alpha1, tau) = elem {
                if alpha == alpha1 {
                    return Some(tau.clone());
                }
            }
        }
        None
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

    fn get_annotation(&self, reference: &Ident) -> Result<&Type> {
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

    fn is_well_formed(&self, ty: &Type) -> bool {
        // TODO
        match ty {
            Type::Literal(_) => true,
            _ => true,
        }
    }
}
