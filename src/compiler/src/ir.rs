/// WebScript uses an IR that only slightly diverges from the AST.
/// The process of lowering the AST to the IR does the name resolution
/// making it easier to typecheck.
/// 
/// The IR exists with the express purpose of allowing:
/// 1. Name resolution
/// 2. Type checking
/// 3. Compiler optimazations
/// ...
use salsa::{self, InternId, InternKey};
use syntax::{FileId, Span};
use syntax::symbol::Symbol;

// We reuse these from the AST
use std::fmt;
pub use syntax::ast::{BinOp, Ident, ParamType, UnOp};

// Uniquely identifies a module
// The file that defines the module. Currently modules can only
// be defined by creating a new file, so ModuleId is technically
// just a light wrapper around FileId.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ModuleId(FileId);

/// Uniquely identifies a function definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct FunctionId {
    module_id: ModuleId,
}

/// Uniquely identifies a component definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ComponentId {
    module_id: ModuleId,
}

/// Uniquely identifies a type definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct TypeId {
    module_id: ModuleId,
}

/// Uniquely identifies a type definition in a module
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct EnumId {
    module_id: ModuleId,
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
    Func {
        body: Block,
        name: Ident,
        params: ParamType,
    },
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

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(value) => write!(f, "{}", value),
            Literal::Bool(value) => write!(f, "{}", value),
            Literal::Number(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Block(pub Vec<StmtId>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Reference {
    Local(LocalId),
    Param,
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExprKind {
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
    // Reference to some local
    Reference(Reference),
    // Block expression
    Block(Block),
    // If expression
    If(ExprId, Block),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum StmtKind {
    Expr(ExprId),
    Local(LocalId),
    Return(ExprId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Local {
    pub name: Symbol,
    pub init: Option<ExprId>,
}
