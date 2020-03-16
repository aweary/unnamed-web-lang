use syntax::{symbol::Symbol, Span};

use std::fmt;
use std::sync::Arc;

use data_structures::arena::Id;
use data_structures::scope_map::Referant;
use data_structures::{Blockable, ControlFlowGraph};



// Reused from the AST
pub use syntax::ast::{AssignOp, BinOp, Ident, Lit, LocalPattern, MatchArm, Ty, UnOp};

pub type ModuleId = Id<Module>;
pub type DefId = Id<Definition>;
pub type StatementId = Id<Statement>;
pub type BlockId = Id<Block>;
pub type ExprId = Id<Expr>;

/// The top-level container for the entire module graph.
#[derive(Debug, Clone)]
pub struct Package {
    /// Version for this package
    version: u32,
    /// The referencable top-level name for the package
    name: Symbol,
    /// The set of modules in the package...
    modules: Vec<ModuleId>,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Local(Arc<Local>),
    State(Arc<Local>),
    Function(Arc<Function>),
    Argument(Arc<Param>),
    Component(Arc<Component>),
}

impl Referant for Binding {}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: LocalPattern,
    pub ty: Option<Box<Ty>>,
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

/// A single module. Modules are just collections of imports
/// and exports; control flow is not allowed at the module level.
/// This makes them effectively just namespaces for a collection
/// of definitions.
#[derive(Debug, Clone)]
pub struct Module {
    // The collection of definitions in this module
    pub definitions: Vec<Definition>,
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub struct Definition {
    pub kind: DefinitionKind,
    pub visibility: DefinitionVisibility,
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub enum DefinitionKind {
    Function(Arc<Function>),
    Component(Arc<Component>),
    Enum,
    Type,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub span: Span,
    pub ty: Ty,
    pub local: LocalPattern,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub params: Vec<Arc<Param>>,
    pub graph: ControlFlowGraph<Statement>,
    pub name: Ident,
    pub span: Span,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Component {
    pub params: Vec<Arc<Param>>,
    pub graph: ControlFlowGraph<Statement>,
    pub name: Ident,
    pub span: Span,
    pub body: Block,
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

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Arc<Statement>>,
}

#[derive(Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
    // ...
}

impl Blockable for Statement {
    fn has_early_exit(&self) -> bool {
        match self.kind {
            StatementKind::Return(_) => true,
            _ => false,
        }
    }
}

impl fmt::Debug for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    // Define a new local variable
    Local(Arc<Local>),
    State(Arc<Local>),
    Expr(Expr),
    // The conditional part of an `if` statement. The body of the
    // condition is represented as a separate basic block.
    BranchingCondition,
    // The conditional part of a loop
    LoopingCondition,
    // Return some value from the current function
    Return(Expr),
    // If statement
    If(IfExpr)
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Option<Ty>,
}

#[derive(Clone, Debug)]
pub struct Template {
    pub instrs: Vec<TemplateInstr>,
    pub kind: TemplateKind,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TemplateKind {
    Static,
    Dynamic,
}

#[derive(Clone, Debug)]
pub enum TemplateInstr {
    // Create a new primitive element
    OpenElement(Symbol),
    // Create a new custom element
    OpenCustomElement(Arc<Component>),
    // Set an attribute on the last open element
    SetAttribute(Symbol, Expr),
    // Set an attribute, except we know the value
    // of the attribute at compile time
    SetStaticAttribute(Symbol, Lit),
    // Embed an expression as a child of an element
    EmbedExpression(Expr),
    // Embed text as a child of an element
    EmbedText(Symbol),
    // Close the last opened element
    CloseElement,
}

#[derive(Clone, Debug)]
pub enum Else {
    Block(Box<Block>),
    // TODO this should be IfExpr but our parser types don't
    // work super well for this right now
    If(Box<IfExpr>),
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub span: Span,
    pub condition: Box<Expr>,
    pub block: Box<Block>,
    pub alt: Option<Else>,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    Array(Vec<Expr>),
    Object(Vec<(Ident, Expr)>),
    Tuple(Vec<Expr>),
    /// Block expression
    Block(Box<Block>),
    /// A binary operation
    Binary(BinOp, Box<Expr>, Box<Expr>),
    /// A unary operation
    Unary(UnOp, Box<Expr>),
    /// Conditional expression, e.g., ternary
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    /// Call expression
    Call(Binding, Vec<Expr>),
    /// Assignment expression
    // TODO the left hand side should be a LeftExpr or something
    Assign(AssignOp, Arc<Local>, Box<Expr>),
    // An update to a state local
    StateUpdate(AssignOp, Arc<Local>, Box<Expr>),
    /// Object member access
    Member(Box<Expr>, Ident),
    /// Object optional member access,
    OptionalMember(Box<Expr>, Ident),
    /// A literal
    Lit(Lit),
    /// A variable reference
    Reference(Binding),
    /// An `if` block with optional `else` block
    If(IfExpr),
    /// For expression
    For(LocalPattern, Box<Expr>, Box<Block>),
    /// An index operation
    Index(Box<Expr>, Box<Expr>),
    /// A `return` with an optional return expression
    Return(Option<Box<Expr>>),
    /// Template
    Template(Template),
    /// Match
    Match(Box<Expr>, Vec<MatchArm>),
    // Function expression
    Func(Arc<Function>),
    // ...
}