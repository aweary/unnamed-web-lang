use serde::{Deserialize, Serialize};

use crate::symbol::Symbol;
use codespan::Span;

pub use crate::ty::{LiteralTy, Ty};

// TODO move these into symbols crate
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn to_str(&self) -> &str {
        &self.name.as_str()
    }
}

// TODO...
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Program {
    pub modules: Vec<Mod>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Mod {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Item {
    pub ident: Ident,
    pub id: NodeId,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ItemKind {
    // A statoc item
    //
    // e.g., `static STATIC_NUM : number = 42`
    Static(Box<Ty>, Box<Expr>),
    // A function declaration
    //
    // e.g., `fn add(a: number, b: number) : number { ... }
    Fn(Box<FnDecl>, FnHeader, Option<Generics>, Box<Block>),
    // An enum definition
    Enum(EnumDef, Option<Generics>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EnumDef {
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Variant {
    pub ident: Ident,
    pub id: NodeId,
    pub span: Span,
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq)]
pub struct Generics {
    // We don't currenly support nested generic types
    pub params: Vec<Ident>,
    // We don't currently support generic functions, but
    // we'll encode this into the AST just in case.
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub has_semi: bool,
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum StmtKind {
    // A local, let binding
    Local(Box<Local>),
    // An item definition, local to some block
    Item(Box<Item>),
    // Expression statement
    // TODO should we differentiate expressions with or without semicolons?
    Expr(Box<Expr>),
    // A while loop
    While(Box<Expr>, Box<Block>),
    // Return statement
    Return(Box<Expr>),
    // Try/catch statement
    TryCatch(Box<Block>, Option<LocalPattern>, Box<Block>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LocalObjectProperty {
    pub key: Ident,
    pub value: LocalPattern,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LocalPattern {
    Ident(Ident, Span),
    Object(Vec<LocalObjectProperty>, Span),
    List(Vec<Ident>, Span),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Local {
    pub id: NodeId,
    pub name: LocalPattern,
    pub ty: Option<Box<Ty>>,
    // Optional initializing expression.
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FnDecl {
    pub params: Vec<Param>,
    pub output: Box<Option<Ty>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct FnHeader {
    pub is_async: bool,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Param {
    pub local: LocalPattern,
    pub ty: Box<Option<Ty>>,
    pub id: NodeId,
    pub span: Span,
    // ...
}

// #[derive(Clone, Debug, Serialize, Deserialize)]
// pub struct Ty {
//     // TODO do we need a name?
//     pub name: Ident,
//     pub id: NodeId,
//     pub kind: TyKind,
//     pub span: Span,
//     pub generics: Option<Generics>,
// }

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TyKind {
    /// Placeholder
    Empty,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum ExprKind {
    Array(Vec<Expr>),
    Tuple(Vec<Expr>),
    // Block expression
    Block(Box<Block>),
    // A binary operation
    Binary(BinOp, Box<Expr>, Box<Expr>),
    // A unary operation
    Unary(UnOp, Box<Expr>),
    // Conditional expression, e.g., ternary
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    // Call expression
    Call(Box<Expr>, Vec<Expr>),
    // Assignment expression
    // TODO the left hand side should be a LeftExpr or something
    Assign(AssignOp, Box<Expr>, Box<Expr>),
    // Object member access
    Member(Box<Expr>, Ident),
    // Object optional member access,
    OptionalMember(Box<Expr>, Ident),
    // A literal
    Lit(Lit),
    // A variable reference
    Reference(Ident),
    // An `if` block with optional `else` block
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
    // For expression
    For(LocalPattern, Box<Expr>, Box<Block>),
    // An index operation
    Index(Box<Expr>, Box<Expr>),
    // A `return` with an optional return expression
    Return(Option<Box<Expr>>),
    // Template
    Template(Template),
    // Match
    Match(Box<Expr>, Vec<MatchArm>),
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MatchArm {}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Template {
    pub id: NodeId,
    pub open: TemplateOpenTag,
    pub close: Option<TemplateCloseTag>,
    pub children: Option<Vec<TemplateChild>>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum TemplateChild {
    Text(Symbol),
    Template(Box<Template>),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TemplateOpenTag {
    pub name: Ident,
    pub attrs: Vec<TemplateAttr>,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TemplateCloseTag {
    pub name: Ident,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct TemplateAttr {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Lit {
    pub span: Span,
    pub kind: LitKind,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum LitKind {
    Bool(Symbol),
    // A number, interned as a symbol. This will be
    // derserialized later on.
    Number(Symbol),
    Str(Symbol),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum BinOp {
    Equals,
    Add,
    Sub,
    Sum,
    Mul,
    Div,
    Mod,
    And,
    Or,
    GreaterThan,
    LessThan,
    Pipeline,
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum UnOp {
    Negate,
    Plus,
    Minus,
    Increment,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum AssignOp {
    Equals,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,
}
