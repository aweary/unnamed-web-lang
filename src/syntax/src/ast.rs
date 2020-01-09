use serde::{Deserialize, Serialize};

use crate::symbol::Symbol;
use codespan::Span;

pub use crate::ty::{LiteralTy, Ty};
use diagnostics::ParseResult as Result;

use std::fmt::{Debug, Error, Formatter};
use std::path::PathBuf;

const DUMMY_NODE_ID: NodeId = NodeId(0);

pub fn expr(kind: ExprKind, span: Span) -> Result<Expr> {
    Ok(Expr {
        id: DUMMY_NODE_ID,
        kind,
        span,
        ty: None,
    })
}

// TODO move these into symbols crate
#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn to_str(&self) -> &str {
        &self.name.as_str()
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self.name)
    }
}

// TODO...
#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct NodeId(pub usize);

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Program {
    pub modules: Vec<Mod>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Mod {
    pub items: Vec<Item>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Item {
    pub ident: Ident,
    pub id: NodeId,
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum ItemKind {
    /// A statoc item
    ///
    /// e.g., `static STATIC_NUM : number = 42`
    Static(Box<Ty>, Box<Expr>),
    /// A function declaration
    ///
    /// e.g., `fn add(a: number, b: number) : number { ... }
    Fn(FnDef),
    /// An enum definition
    Enum(EnumDef, Option<Generics>),
    /// Type definition for records
    Type(Box<TypeDef>),
    /// Import declaration
    Import(Box<Import>),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Import {
    pub name: Ident,
    pub span: Span,
    pub path: ImportPath,
}

impl Import {
    pub fn to_path_buf(&self) -> PathBuf {
        self.clone().path.path
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Serialize, Deserialize)]
pub struct ImportPath {
    pub path: PathBuf,
    pub span: Span,
}

impl Debug for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self.path)
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TypeDef {
    pub name: Ident,
    pub span: Span,
    pub generics: Option<Generics>,
    pub properties: Vec<TypeProperty>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TypeProperty {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum ParamType {
    // No paramters
    Empty,
    // A single paramter
    Single(Param),
    // >=2 paramters
    Multi(Vec<Param>),
}

impl IntoIterator for ParamType {
    type Item = Param;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        use ParamType::*;
        match self {
            Empty => vec![].into_iter(),
            Single(param) => vec![param].into_iter(),
            Multi(params) => params.into_iter(),
        }
    }
}

/// A function definition
#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct FnDef {
    pub name: Ident,
    pub params: ParamType,
    pub body: Box<Block>,
    pub return_ty: Ty,
    pub is_async: bool,
    pub generics: Option<Generics>,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct EnumDef {
    pub variants: Vec<Variant>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Variant {
    pub ident: Ident,
    pub id: NodeId,
    pub span: Span,
    // ...
}

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Generics {
    // We don't currenly support nested generic types
    pub params: Vec<Ident>,
    // We don't currently support generic functions, but
    // we'll encode this into the AST just in case.
    // ...
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub has_semi: bool,
    // ...
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct LocalObjectProperty {
    pub key: Ident,
    pub value: LocalPattern,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum LocalPattern {
    Ident(Ident, Span),
    Object(Vec<LocalObjectProperty>, Span),
    List(Vec<Ident>, Span),
}

impl Into<Symbol> for LocalPattern {
    fn into(self) -> Symbol {
        match self {
            LocalPattern::Ident(ident, _) => ident.name,
            _ => todo!("Cannot make destructured LocalPattern into Symbol"),
        }
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Local {
    pub id: NodeId,
    pub name: LocalPattern,
    pub ty: Option<Box<Ty>>,
    // Optional initializing expression.
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct FnDecl {
    pub params: Vec<Param>,
    pub output: Box<Option<Ty>>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct FnHeader {
    pub is_async: bool,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Param {
    pub local: LocalPattern,
    pub ty: Ty,
    pub id: NodeId,
    pub span: Span,
    // ...
}

impl Param {
    pub fn name(&self) -> Symbol {
        match &self.local {
            LocalPattern::Ident(ident, _) => ident.name.clone(),
            _ => todo!("Cant get name for destructure yet"),
        }
    }
}

// #[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
// pub struct Ty {
//     // TODO do we need a name?
//     pub name: Ident,
//     pub id: NodeId,
//     pub kind: TyKind,
//     pub span: Span,
//     pub generics: Option<Generics>,
// }

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum TyKind {
    /// Placeholder
    Empty,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Option<Ty>,
    // ...
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
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
    Call(Box<Expr>, Vec<Expr>),
    /// Assignment expression
    // TODO the left hand side should be a LeftExpr or something
    Assign(AssignOp, Box<Expr>, Box<Expr>),
    /// Object member access
    Member(Box<Expr>, Ident),
    /// Object optional member access,
    OptionalMember(Box<Expr>, Ident),
    /// A literal
    Lit(Lit),
    /// A variable reference
    Reference(Ident),
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
    /// Function expression
    Func(Box<FnDef>),
    // ...
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum Else {
    Block(Box<Block>),
    // TODO this should be IfExpr but our parser types don't
    // work super well for this right now
    If(Box<Expr>),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub block: Box<Block>,
    pub alt: Option<Else>,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct MatchArm {}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Template {
    pub id: NodeId,
    pub open: TemplateOpenTag,
    pub close: Option<TemplateCloseTag>,
    pub children: Option<Vec<TemplateChild>>,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum TemplateChild {
    Text(Symbol),
    Template(Box<Template>),
    Expr(Box<Expr>),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TemplateOpenTag {
    pub name: Ident,
    pub attrs: Vec<TemplateAttr>,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TemplateCloseTag {
    pub name: Ident,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct TemplateAttr {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub struct Lit {
    pub span: Span,
    pub kind: LitKind,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum LitKind {
    Bool(Symbol),
    // A number, interned as a symbol. This will be
    // derserialized later on.
    Number(Symbol),
    Str(Symbol),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum BinOp {
    Equals,
    DblEquals,
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

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum UnOp {
    Negate,
    Plus,
    Minus,
    Increment,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Serialize, Deserialize)]
pub enum AssignOp {
    Equals,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,
}
