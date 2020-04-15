
use crate::symbol::Symbol;

pub use crate::ty::{LiteralTy, Ty};
use diagnostics::ParseResult as Result;
use source::diagnostics::Span;

use std::fmt::{Debug, Error, Formatter};
use std::path::PathBuf;

use graphql_parser::query::Document;

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
#[derive(Clone, PartialEq)]
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
#[derive(Clone, Debug)]
pub struct NodeId(pub usize);

#[derive(Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
}

#[derive(Clone, Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum ItemKind {
    Constant(Constant),
    /// A function declaration
    ///
    /// e.g., `fn add(a: number, b: number) : number { ... }
    Fn(FnDef),
    /// A component definition
    Component(ComponentDef),
    /// An enum definition
    Enum(EnumDef),
    /// Type definition for records
    Type(Box<TypeDef>),
    /// Import declaration
    Import(Box<Import>),
    /// Exported declaration
    Export(Box<Item>),
}

#[derive(Clone, Debug)]
pub struct Constant {
    pub name: Ident,
    pub ty: Ty,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Import {
    pub specifiers: Vec<ImportSpecifier>,
    pub span: Span,
    pub path: ImportPath,
}

impl Import {
    pub fn to_path_buf(&self) -> PathBuf {
        self.clone().path.path
    }

    pub fn resolve(&self, base: &PathBuf) -> Result<PathBuf> {
        use path_dedot::*;
        let path = &self.path.path;
        let path = base.join(path).parse_dot().unwrap();
        Ok(path)
    }
}

#[derive(Clone, Debug)]
pub struct ImportSpecifier {
    // The name of the item being imported
    pub ident: Ident,
    // An optional alias using the `{A as B}` syntax
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Clone)]
pub struct ImportPath {
    pub path: PathBuf,
    pub span: Span,
}

impl Debug for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self.path)
    }
}

#[derive(Clone, Debug)]
pub struct TypeDef {
    pub name: Ident,
    pub span: Span,
    pub generics: Option<Generics>,
    pub properties: Vec<TypeProperty>,
}

#[derive(Clone, Debug)]
pub struct TypeProperty {
    pub name: Ident,
    pub ty: Ty,
}

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub struct FnDef {
    pub name: Ident,
    pub params: ParamType,
    pub body: Box<Block>,
    pub return_ty: Ty,
    pub is_async: bool,
    pub generics: Option<Generics>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum LambdaBody {
    Block(Box<Block>),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct Lambda {
    pub body: LambdaBody,
    pub params: ParamType,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ComponentDef {
    pub name: Ident,
    pub params: ParamType,
    pub body: Box<Block>,
    pub return_ty: Ty,
    pub generics: Option<Generics>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct EnumDef {
    pub name: Ident,
    pub variants: Vec<Variant>,
    pub parameters: Option<Vec<Ident>>,
    pub span: Span,
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

#[derive(Clone, Debug)]
pub struct Generics {
    // We don't currenly support nested generic types
    pub params: Vec<Ident>,
    // We don't currently support generic functions, but
    // we'll encode this into the AST just in case.
    // ...
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub has_semi: bool,
    // ...
}

#[derive(Clone, Debug)]
pub enum StmtKind {
    // A local, let binding
    Local(Box<Local>),
    // A state definition. Like a local binding, but more restricted.
    State(Box<Local>),
    // An item definition, local to some block
    Item(Box<Item>),
    // Expression statement
    // TODO should we differentiate expressions with or without semicolons?
    Expr(Box<Expr>),
    // A while loop
    While(Box<Expr>, Box<Block>),
    // If statement
    If(IfExpr),
    // Return statement
    Return(Box<Expr>),
    // Try/catch statement
    TryCatch(Box<Block>, Option<LocalPattern>, Box<Block>),
}

#[derive(Clone, Debug)]
pub struct LocalObjectProperty {
    pub key: Ident,
    pub value: LocalPattern,
    pub span: Span,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct Local {
    pub id: NodeId,
    pub name: LocalPattern,
    pub ty: Option<Box<Ty>>,
    // Optional initializing expression.
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct FnDecl {
    pub params: Vec<Param>,
    pub output: Box<Option<Ty>>,
}

#[derive(Clone, Debug)]
pub struct FnHeader {
    pub is_async: bool,
}

#[derive(Clone, Debug)]
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

// #[derive(Clone, Debug, )]
// pub struct Ty {
//     // TODO do we need a name?
//     pub name: Ident,
//     pub id: NodeId,
//     pub kind: TyKind,
//     pub span: Span,
//     pub generics: Option<Generics>,
// }

#[derive(Clone, Debug)]
pub enum TyKind {
    /// Placeholder
    Empty,
}

#[derive(Clone, Debug)]
pub struct Expr {
    pub id: NodeId,
    pub kind: ExprKind,
    pub span: Span,
    pub ty: Option<Ty>,
    // ...
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
    /// Lambda function expression
    Lambda(Lambda),
    /// GraphQL query
    Query(Box<Document>),
    // ...
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
pub struct MatchArm {
    pub test: Expr,
    pub consequent: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Template {
    pub id: NodeId,
    pub open: TemplateOpenTag,
    pub close: Option<TemplateCloseTag>,
    pub children: Option<Vec<TemplateChild>>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum TemplateChild {
    Text(Symbol),
    Template(Box<Template>),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct TemplateOpenTag {
    pub name: Ident,
    pub attrs: Vec<TemplateAttr>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TemplateCloseTag {
    pub name: Ident,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TemplateAttr {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Lit {
    pub span: Span,
    pub kind: LitKind,
}

#[derive(Clone, Debug)]
pub enum LitKind {
    Bool(Symbol),
    // A number, interned as a symbol. This will be
    // derserialized later on.
    Number(Symbol),
    Str(Symbol),
}

#[derive(Clone, Debug)]
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
    BinOr,
    BinAdd,
    // ...
}

#[derive(Clone, Debug)]
pub enum UnOp {
    Negate,
    Plus,
    Minus,
    Increment,
}

#[derive(Clone, Debug)]
pub enum AssignOp {
    Equals,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,
}
