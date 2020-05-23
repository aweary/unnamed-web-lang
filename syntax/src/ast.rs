use crate::symbol::Symbol;

use diagnostics::ParseResult as Result;
use serde::{Deserialize, Serialize};
use source::diagnostics::Span;

use std::fmt::{Debug, Error, Formatter};
use std::path::PathBuf;



/// How types are represented in the AST. Intrinsic primitive types like `number`
/// and `string` are identified at parsing. We don't alias types to use these names.
#[derive(Serialize, Deserialize, Debug, Clone)]
pub enum TypeKind {
    List(Box<Type>),
    Tuple(Vec<Type>),
    Record(Vec<RecordTypeField>),
    Function(Box<Type>, Box<Type>),
    Reference(Ident, Option<Vec<Ident>>),
}

#[derive(Serialize, Deserialize, Clone)]
pub struct Type {
    pub span: Span,
    pub kind: TypeKind,
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }

}


#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TypeDef {
    pub name: Ident,
    pub ty: Type,
    pub parameters: Option<Vec<Ident>>,
    pub span: Span,
}

pub fn expr(kind: ExprKind, span: Span) -> Result<Expr> {
    Ok(Expr { kind, span })
}

// TODO move these into symbols crate
#[derive(Serialize, Deserialize, Clone, PartialEq)]
pub struct Ident {
    pub symbol: Symbol,
    pub span: Span,
}

impl Ident {
    pub fn to_str(&self) -> &str {
        &self.symbol.as_str()
    }
}

impl Debug for Ident {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self.symbol)
    }
}

// TODO...
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct NodeId(pub usize);

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Program {
    pub modules: Vec<Module>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum ItemKind {
    Constant(Constant),
    /// A function declaration
    ///
    /// e.g., `fn add(a: number, b: number) : number { ... }
    Fn(Function),
    /// A component definition
    Component(Component),
    /// An enum definition
    Enum(EnumDef),
    /// Type definition for records
    Type(TypeDef),
    /// Import declaration
    Import(Box<Import>),
    /// Exported declaration
    Export(Box<Item>),
    /// Struct definition
    Struct(Struct),
}

//  TODO(brandondail) currently just supports function type aliases
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeAlias {
    pub name: Ident,
    pub parameters: Vec<Type>,
    pub return_ty: Type,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Constant {
    pub name: Ident,
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
        use path_dedot::ParseDot;
        let path = &self.path.path;
        let path = base.join(path).parse_dot().unwrap();
        Ok(path)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ImportSpecifier {
    // The name of the item being imported
    pub ident: Ident,
    // An optional alias using the `{A as B}` syntax
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone)]
pub struct ImportPath {
    pub path: PathBuf,
    pub span: Span,
}

impl Debug for ImportPath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::result::Result<(), Error> {
        write!(f, "{:?}", self.path)
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeDefinition {
    pub kind: TypeDefinitionKind,
    pub span: Span,
}


/// The kinds of types that can be declared/named
#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TypeDefinitionKind {
    /// An alias to another type
    Alias(Ident),
    /// A Tuple
    Tuple(Vec<Type>),
    /// A function
    Function(Vec<Type>, Type),
    /// An object/record type
    Record(Vec<RecordTypeField>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct RecordTypeField {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TypeProperty {
    pub name: Ident,
    pub ty: Type,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
        use ParamType::{Empty, Multi, Single};
        match self {
            Empty => vec![].into_iter(),
            Single(param) => vec![param].into_iter(),
            Multi(params) => params.into_iter(),
        }
    }
}


#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Struct {
    pub name: Ident,
    pub span: Span,
}

/// A function definition
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Function {
    pub name: Ident,
    pub params: ParamType,
    pub body: Block,
    pub return_ty: Option<Type>,
    pub is_async: bool,
    pub generics: Option<Generics>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum LambdaBody {
    Block(Box<Block>),
    Expr(Box<Expr>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Lambda {
    pub body: LambdaBody,
    pub params: ParamType,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Component {
    pub name: Ident,
    pub params: ParamType,
    pub body: Block,
    pub return_ty: Option<Type>,
    pub generics: Option<Generics>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct EnumDef {
    pub name: Ident,
    pub variants: Vec<Variant>,
    pub parameters: Option<Vec<Ident>>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Variant {
    // The name of the variant
    pub ident: Ident,
    // The input types for tuple variants
    pub fields: Option<Vec<Type>>,
    // TODO discriminants should be restricted to simple types like numbers and strings
    pub discriminant: Option<Expr>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Generics {
    // We don't currenly support nested generic types
    pub params: Vec<Ident>,
    // We don't currently support generic functions, but
    // we'll encode this into the AST just in case.
    // ...
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub id: NodeId,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Stmt {
    pub id: NodeId,
    pub kind: StmtKind,
    pub span: Span,
    pub has_semi: bool,
    // ...
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
    // Throw
    Throw(Expr),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct LocalObjectProperty {
    pub key: Ident,
    pub value: LocalPattern,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone)]
pub enum LocalPattern {
    Ident(Ident, Span),
    Object(Vec<LocalObjectProperty>, Span),
    List(Vec<Ident>, Span),
}

impl Debug for LocalPattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LocalPattern::Ident(ident, _) => write!(f, "{:?}", ident),
            LocalPattern::Object(_, _) => write!(f, "LocalPattern::Object"),
            LocalPattern::List(idents, _) => write!(f, "{:?}", idents),
        }
    }
}

impl Into<Symbol> for LocalPattern {
    fn into(self) -> Symbol {
        match self {
            LocalPattern::Ident(ident, _) => ident.symbol,
            _ => todo!("Cannot make destructured LocalPattern into Symbol"),
        }
    }
}

impl Into<Ident> for LocalPattern {
    fn into(self) -> Ident {
        match self {
            LocalPattern::Ident(ident, _) => ident,
            _ => todo!("Cannot make destructured LocalPattern into Symbol"),
        }
    }
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Local {
    pub id: NodeId,
    pub name: LocalPattern,
    pub ty: Option<Type>,
    // Optional initializing expression.
    pub init: Option<Box<Expr>>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct FnDecl {
    pub params: Vec<Param>,
    pub output: Box<Option<Type>>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct FnHeader {
    pub is_async: bool,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Param {
    pub local: LocalPattern,
    pub ty: Option<Type>,
    pub id: NodeId,
    pub span: Span,
    // ...
}

impl Param {
    pub fn name(&self) -> Symbol {
        match &self.local {
            LocalPattern::Ident(ident, _) => ident.symbol.clone(),
            _ => todo!("Cant get name for destructure yet"),
        }
    }
}

// #[derive(Serialize, Deserialize, Clone, Debug, )]
// pub struct Ty {
//     // TODO do we need a name?
//     pub name: Ident,
//     pub id: NodeId,
//     pub kind: TyKind,
//     pub span: Span,
//     pub generics: Option<Generics>,
// }

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TyKind {
    /// Placeholder
    Empty,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Argument {
    pub span: Span,
    pub name: Option<Ident>,
    pub value: Expr,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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
    Call(Box<Expr>, Vec<Argument>),
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
    Func(Box<Function>),
    /// Lambda function expression
    Lambda(Lambda),
    /// Trailing closure function call
    TrailingClosure(Box<Expr>, Block),
    // ...
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum Else {
    Block(Box<Block>),
    // TODO this should be IfExpr but our parser types don't
    // work super well for this right now
    If(Box<IfExpr>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct IfExpr {
    pub span: Span,
    pub condition: Box<Expr>,
    pub block: Box<Block>,
    pub alt: Option<Else>,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct MatchArm {
    pub test: Expr,
    pub consequent: Expr,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Template {
    pub id: NodeId,
    pub open: TemplateOpenTag,
    pub close: Option<TemplateCloseTag>,
    pub children: Option<Vec<TemplateChild>>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum TemplateChild {
    Text(Symbol),
    Template(Box<Template>),
    Expr(Box<Expr>),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TemplateOpenTag {
    pub name: Ident,
    pub attrs: Vec<TemplateAttr>,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TemplateCloseTag {
    pub name: Ident,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct TemplateAttr {
    pub name: Ident,
    pub value: Expr,
    pub span: Span,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct Lit {
    pub span: Span,
    pub kind: LitKind,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum LitKind {
    Bool(Symbol),
    // A number, interned as a symbol. This will be
    // derserialized later on.
    Number(Symbol),
    Str(Symbol),
}

#[derive(Serialize, Deserialize, Clone, Debug)]
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

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum UnOp {
    Negate,
    Plus,
    Minus,
    Increment,
}

#[derive(Serialize, Deserialize, Clone, Debug)]
pub enum AssignOp {
    Equals,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,
}
