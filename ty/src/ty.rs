//! Definitions for how types are represented in the HIR and type checker.
//! This is shared between the `hir` and `typecheck` module, as the lowering step
//! can resolve some basic type information.



use internment::Intern;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Existential(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(pub u16);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralType {
    Number,
    String,
    Boolean,
}

#[macro_export]
macro_rules! number {
    () => {
        Type::Literal(LiteralType::Number).into()
    };
}

#[macro_export]
macro_rules! boolean {
    () => {
        Type::Literal(LiteralType::Boolean).into()
    };
}

#[macro_export]
macro_rules! string {
    () => {
        Type::Literal(LiteralType::String).into()
    };
}

// Set of possible types that the user can define or reference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    /// Primitive literal types
    Literal(LiteralType),
    /// An unknown type that will need to be inferred
    Existential(Existential),
    /// Existentials are progressively solved as types are checked.
    SolvableExistential(Existential, Option<Intern<Type>>),
    /// The empty type
    Unit,
    /// Function abstraction
    Function(Vec<Intern<Type>>, Intern<Type>),
    // A 2-tuple of types
    Pair(Intern<Type>, Intern<Type>),
    // An n-tuple of types
    Tuple(Vec<Intern<Type>>),
    /// An array of T items
    List(Intern<Type>),
    Quantification(Variable, Intern<Type>),
    Variable(Variable),
}

impl Type {
    pub fn is_monotype(&self) -> bool {
        match self {
            Type::Quantification(_, _) => false,
            Type::Function(t1, t2) => {
                t1.iter().all(|ty| ty.is_monotype()) && t2.is_monotype()
            }
            _ => true,
        }
    }
}
