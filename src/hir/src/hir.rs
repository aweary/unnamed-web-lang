use codespan::FileId;
use syntax::{symbol::Symbol, Span};

use std::fmt;
use std::sync::Arc;

use crate::control_flow_graph::ControlFlowGraph;
// use crate::module_graph::ModuleGraph;

use syntax::ast::Ident;
// pub use crate::scope::{Binding, Reference};

/// The top-level container for the entire module graph.
#[derive(Debug, Clone)]
pub struct Package {
    /// Version for this package
    version: u32,
    /// The referencable top-level name for the package
    name: Symbol,
    /// The set of modules in the package...
    modules: Vec<Module>,
    // module_graph: ModuleGraph,
    // ...
}

/// A single module. Modules are just collections of imports
/// and exports; control flow is not allowed at the module level.
/// This makes them effectively just namespaces for a collection
/// of definitions.
#[derive(Debug, Clone)]
pub struct Module {
    // The file this module is defined in.
    file_id: FileId,
    // The collection of definitions in this module
    // definitions: Vec<&'hir Definition>,
    // ...
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub struct Definition {
    kind: DefinitionKind,
    visibility: DefinitionVisibility,
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub enum DefinitionKind {
    Function(Arc<Function>),
    Component,
    Enum,
    Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) graph: ControlFlowGraph,
    pub name: Ident,
    pub span: Span,
}

/// The visibility of a definition, relative to other definitions.
/// Private definitions are only visible (referenceable) to other
/// definitions in the same module. Public definitions can be imported
/// by any other module.
#[derive(Debug, Clone)]
pub enum DefinitionVisibility {
    Private,
    Public,
}

#[derive(Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
    // ...
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    // Define a new local variable
    LocalDefinition,
    // The conditional part of an `if` statement. The body of the
    // condition is represented as a separate basic block.
    BranchingCondition,
    // The conditional part of a loop
    LoopingCondition,
    // Return some value from the current function
    Return,
}

#[derive(Debug, Clone)]
pub struct Expr {
    // ...
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A phi (ϕ) function is a special function that is inserted
    /// when creating the control-flow-graph for a block of code.
    /// It is an internal-only construct.
    PhiFunction,
}
