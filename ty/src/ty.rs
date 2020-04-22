//! Definitions for how types are represented in the HIR and type checker.
//! This is shared between the `hir` and `typecheck` module, as the lowering step
//! can resolve some basic type information.

use syntax::symbol::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Existential(pub u16);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LiteralType {
    Number,
    String,
    Boolean,
}

// Set of possible types that the user can define or reference
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Primitive literal types
    Literal(LiteralType),
    /// An unknown type that will need to be inferred
    Existential(Existential),
    /// Existentials are progressively solved as types are checked.
    SolvableExistential(Existential, Option<Box<Type>>),
    // Placeholder while we figure out existentials
    UnknownExistential_DO_NOT_USE,
    /// The empty type
    Unit,
    /// Function abstraction.
    Function(Box<Type>, Box<Type>),
    // A 2-tuple of types
    Pair(Box<Type>, Box<Type>),
    /// An array of T items
    List(Box<Type>),
    Quantification(Symbol, Box<Type>),
    Variable(Symbol),
    // TODO need to fill these out later. Have to figure out how to represent them
    Enum,
    Record,
    Parameter,
}

impl Type {
    pub fn function(input: Type, output: Type) -> Self {
        Type::Function(Box::new(input), Box::new(output)) 
    }

    pub fn pair(a: Type, b: Type) -> Self {
        Type::Pair(Box::new(a), Box::new(b)) 
    }

    pub fn is_monotype(&self) -> bool {
        match self {
            Type::Quantification(_, _) => false,
            Type::Function(t1, t2) => t1.is_monotype() && t2.is_monotype(),
            _ => true
        }

    }
}