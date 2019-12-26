/// WebScript uses an IR that only slightly diverges from the AST.
/// The process of lowering the AST to the IR does the name resolution
/// making it easier to typecheck.
use salsa::{self, InternId, InternKey};
use syntax::{Span, FileId};

// We reuse these from the AST
pub use syntax::ast::{BinOp, UnOp};

// A module is a collection of definitions, some exported and some not
struct Module_ {
    // The definitions defined in this module
    defs: Vec<DefId_>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DefId_ {
    Function(FunctionId),
    Component(ComponentId),
    Type(TypeId),
    Enum(EnumId),
}

// Uniquely identifies a module
// The file that defines the module. Currently modules can only
    // be defined by creating a new file, so ModuleId is technically
    // just a light wrapper around FileId.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ModuleId(FileId);

/// A function definition. It contains a set of statements and expressions.
struct FunctionDef {
    span: Span,
    body: Block,
}

// A block
struct Block_ {
    stmts: Vec<StmtId>,
}

/// Uniquely identifies a function definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionId {
    module_id: ModuleId,
}

/// Uniquely identifies a component definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ComponentId {
    module_id: ModuleId
}

/// A record type definition
struct TypeDef {
  // ...
}

/// Uniquely identifies a type definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TypeId {
    module_id: ModuleId
}

/// An enum definition.
struct EnumDef {

}
/// Uniquely identifies a type definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct EnumId {
    module_id: ModuleId
}

/// Macro for creating an internable ID
macro_rules! intern_id {
    ($name: ident) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
        pub struct $name(pub u32);

        impl InternKey for $name {
            fn from_intern_id(v: InternId) -> Self {
                $name(v.as_u32())
            }
            fn as_intern_id(&self) -> InternId {
                InternId::from(self.0)
            }
        }
    };
}

intern_id!(SpanId);
intern_id!(ExprId);
intern_id!(DefId);
intern_id!(StmtId);
intern_id!(LocalId);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Module {
    pub items: Vec<DefId>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DefKind {
    Func { body: Block },
    Type,
    Enum,
    Import,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Def {
    pub kind: DefKind,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Literal {
    String(String),
    Bool(bool),
    Number(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block(pub Vec<StmtId>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    /// Literal expression
    Literal(Literal),
    /// Binary expression
    Binary {
        lhs: ExprId,
        rhs: ExprId,
        op: BinOp,
    },
    // Array literal expression
    Array(Vec<ExprId>),
    // Unary expression
    Unary(UnOp, ExprId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Stmt {
    Expr(ExprId),
    Local(LocalId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Local {
    // pub name: Symbol,
    pub init: Option<ExprId>,
}
