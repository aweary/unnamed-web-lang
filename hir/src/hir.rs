use syntax::{symbol::Symbol};

use source::diagnostics::Span;

use std::fmt;
use std::sync::Arc;
use std::sync::Mutex;

use data_structures::arena::Id;
use data_structures::scope_map::Referant;
use data_structures::{Blockable, ControlFlowGraph};
use source::FileId;

use edit_distance::edit_distance;

use std::path::PathBuf;

// Reused from the AST
pub use syntax::ast::{
    AssignOp, BinOp, Ident, ImportSpecifier, Lit, LitKind, LocalPattern, UnOp, TypeDef,
};

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
pub enum BuiltInType {
    String,
    Number,
    Boolean,
    Array,
}

#[derive(Debug, Clone)]
pub enum BuiltIn {
    /// The `_` identifier, which has special semantics
    EmptyBinding,
    Type(BuiltInType),
}

#[derive(Debug, Clone)]
pub enum Binding {
    BuiltIn(BuiltIn),
    Local(Arc<Local>),
    State(Arc<Local>),
    Function(Arc<Mutex<Function>>),
    Argument(Arc<Param>),
    Component(Arc<Component>),
    Import(Arc<Mutex<Import>>),
    Constant(Arc<Constant>),
    Type(Arc<TypeDef>),
}

impl Referant for Binding {}

#[derive(Clone, Debug)]
pub struct Local {
    pub name: LocalPattern,
    pub ty: Option<Ty>,
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

/// A single module. Modules are just collections of imports
/// and exports; control flow is not allowed at the module level.
/// This makes them effectively just namespaces for a collection
/// of definitions.
#[derive(Debug, Clone)]
pub struct Module {
    // The list of imports
    pub imports: Vec<Arc<Mutex<Import>>>,
    // The collection of definitions in this module
    pub definitions: Vec<Definition>,
    // The absolute path for this module
    // pub path: PathBuf,
    // The unique file ID for accessing this module's source
    pub file: FileId,
}

impl Module {
    pub fn resolve_export(&self, name: &Ident) -> Option<Definition> {
        for def in &self.definitions {
            if def.visibility == DefinitionVisibility::Public {
                match &def.kind {
                    DefinitionKind::Function(fndef) => {
                        let fndef = fndef.lock().unwrap();
                        if fndef.name.name == name.name {
                            return Some(def.clone());
                        }
                    }
                    DefinitionKind::Component(compdef) => {
                        if compdef.name.name == name.name {
                            return Some(def.clone());
                        }
                    }
                    DefinitionKind::Constant(constant)  => {
                        if constant.name.name == name.name {
                            return Some(def.clone());
                        }
                    }
                    _ => unimplemented!(),
                }
            }
        }
        None
    }

    pub fn resolve_similar_export(&self, export_ident: &Ident) -> Option<&Definition> {
        let mut similar_export = None;
        let mut max_edit_distance = std::usize::MAX;
        for def in &self.definitions {
            let def_ident = def.name();
            let distance = edit_distance(export_ident.name.as_str(), def_ident.name.as_str());
            if distance < max_edit_distance {
                max_edit_distance = distance;
                similar_export = Some(def);
            }
        }
        if max_edit_distance < 5 {
            similar_export
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct ImportPath {
    pub resolved: PathBuf,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ReferenceTy {
    pub ident: Ident,
    pub typedef: Arc<TypeDef>,
    pub type_args: Option<Vec<Ty>>,
}

#[derive(Debug, Clone)]
pub enum Ty {
    // Built-in types, with optional type arguments.
    BuiltIn(BuiltInType, Option<Vec<Ty>>),
    // A reference to some user-defined definition
    Reference(ReferenceTy),
    // A placeholder for a type that we don't have information about.
    // This will need to be inferred during type checking.
    Unknown,
    // The empty type
    Unit,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub path: ImportPath,
    pub name: Ident,
    pub span: Span,
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub struct Definition {
    pub kind: DefinitionKind,
    pub visibility: DefinitionVisibility,
    pub span: Span,
}

impl Definition {
    pub fn name(&self) -> Ident {
        match &self.kind {
            DefinitionKind::Function(fndef) => {
                let fndef = fndef.lock().unwrap();
                fndef.name.clone()
            }
            DefinitionKind::Component(compdef) => compdef.name.clone(),
            DefinitionKind::Constant(constant) => constant.name.clone(),
            _ => unimplemented!(),
        }
    }
}

/// A definition for some nameable item.
#[derive(Debug, Clone)]
pub enum DefinitionKind {
    Function(Arc<Mutex<Function>>),
    Component(Arc<Component>),
    Constant(Arc<Constant>),
    Enum(Arc<EnumDef>),
    Type(Arc<TypeDef>),
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub name: Ident,
    pub ty: Ty,
    pub value: Expr,
    pub span: Span,
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
pub enum LambdaBody {
    Block(Box<Block>),
    Expr(Box<Expr>),
}

#[derive(Debug, Clone)]
pub struct Lambda {
    pub params: Vec<Arc<Param>>,
    pub graph: ControlFlowGraph<Statement>,
    pub body: LambdaBody,
    pub span: Span, 
}

#[derive(Debug, Clone)]
pub struct Component {
    pub params: Vec<Arc<Param>>,
    pub graph: ControlFlowGraph<Statement>,
    pub name: Ident,
    pub span: Span,
    pub body: Block,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: Ident,
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug)]
pub struct Variant {
    // The name of the variant
    pub ident: Ident,
    // The input types for tuple variants
    pub fields: Option<Vec<Ty>>,
    // TODO discriminants should be restricted to simple types like numbers and strings
    pub discriminant: Option<Expr>,
    pub span: Span,
}

/// The visibility of a definition, relative to other definitions.
/// Private definitions are only visible (referenceable) to other
/// definitions in the same module. Public definitions can be imported
/// by any other module.
#[derive(Debug, Clone, PartialEq)]
pub enum DefinitionVisibility {
    Private,
    Public,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Arc<Mutex<Statement>>>,
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
    If(IfExpr),
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
pub struct MatchArm {
    pub test: Expr,
    pub consequent: Expr,
    pub span: Span,
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
    /// Member call expression
    MemberCall(Box<Expr>, Ident, Vec<Expr>),
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
    Reference(Ident, Binding),
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
    Func(Arc<Mutex<Function>>),
    // Lamda expression
    Lambda(Lambda),
    // ...
}
