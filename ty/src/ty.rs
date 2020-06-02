//! Definitions for how types are represented in the HIR and type checker.
//! This is shared between the `hir` and `typecheck` module, as the lowering step
//! can resolve some basic type information.

use internment::Intern;

use crate::effects::EffectType;
use std::fmt::Display;
use syntax::symbol::Symbol;

pub type InternType = Intern<Type>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Existential(pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Variable(pub u16);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LiteralType {
    Number,
    String,
    Bool,
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralType::Number => write!(f, "number"),
            LiteralType::String => write!(f, "string"),
            LiteralType::Bool => write!(f, "boolean"),
        }
    }
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
        Type::Literal(LiteralType::Bool).into()
    };
}

#[macro_export]
macro_rules! string {
    () => {
        Type::Literal(LiteralType::String).into()
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Parameter {
    pub ty: InternType,
    /// We need to include a name in the type to
    /// support named arguments.
    pub name: Option<Symbol>,
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
    Function {
        parameters: Vec<Parameter>,
        out: InternType,
        effect: EffectType,
    },
    // Function(Vec<Intern<Type>>, Intern<Type>),
    // A 2-tuple of types
    Pair(Intern<Type>, Intern<Type>),
    // An n-tuple of types
    Tuple(Vec<Intern<Type>>),
    /// An array of T items
    List(Intern<Type>),
    Quantification(Vec<Variable>, Intern<Type>),
    Variable(Variable),
    // Component abstraction
    Component {
        parameters: Vec<InternType>,
        // TODO this needs to be constrained to valid return
        // types for components
        return_ty: InternType,
    },
    // Algebraic data type, used to represent enums
    // TODO use UniqueName here?
    Adt { variants: Vec<u32> },
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Literal(lit) => write!(f, "{}", lit),
            Type::Existential(a) => write!(f, "∀{:?}", a),
            Type::SolvableExistential(a, b) => {
                if let Some(ty) = b {
                    write!(f, "{}", ty)
                } else {
                    write!(f, "∀{:?}", a)
                }
            }
            Type::Unit => write!(f, "unit"),
            Type::Function { .. } => write!(f, "function (TODO)"),
            Type::Pair(_, _) => write!(f, "pair (TODO)"),
            Type::Tuple(_) => write!(f, "tuple (TODO)"),
            Type::List(_) => write!(f, "list (TODO)"),
            Type::Quantification(_, _) => write!(f, "Quantification (TODO)"),
            Type::Variable(_) => write!(f, "variable (TODO)"),
            Type::Component { .. } => write!(f, "Component (TODO)"),
            Type::Adt { .. } => write!(f, "Component (TODO)"),
        }
    }
}

impl Type {
    pub fn new_function(
        parameters: Vec<Parameter>,
        out: InternType,
    ) -> InternType {
        Type::Function {
            parameters,
            out,
            effect: EffectType::default(),
        }
        .into()
    }

    pub fn new_quantification(
        variables: Vec<Variable>,
        ty: InternType,
    ) -> InternType {
        Type::Quantification(variables, ty).into()
    }

    pub fn new_function_with_effect(
        parameters: Vec<Parameter>,
        out: InternType,
        effect: EffectType,
    ) -> InternType {
        Type::Function {
            parameters,
            out,
            effect,
        }
        .into()
    }

    pub fn apply_effect(&mut self, new_effect: EffectType) {
        if let Type::Function { effect, .. } = self {
            *effect = new_effect
        }
    }

    pub fn is_monotype(&self) -> bool {
        match self {
            Type::Quantification(_, _) => false,
            Type::Function {
                parameters, out, ..
            } => {
                parameters.iter().all(|param| param.ty.is_monotype())
                    && out.is_monotype()
            }
            _ => true,
        }
    }
}
