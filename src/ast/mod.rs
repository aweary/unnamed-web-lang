use crate::symbol::Symbol;

// The root node of the AST.
pub struct Program {
    modules: Vec<Module>
}

// An individual module. Currently the only way to define
// a module is with a new file, so this maps directly to the filesystem
// layout of the project.
pub struct Module {

}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1,
    CONDITIONAL = 2,
    SUM = 3,
    PRODUCT = 4,
    COMPARE = 5,
    PREFIX = 6,
}

#[derive(Debug)]
pub struct Expr {
    // span: Span,
    kind: Box<ExprKind>,
}

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            kind: Box::new(kind)
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Op {
  Add,
  Sub,
  Div,
  Mul,
  Mod,
  And,
  Or,
}

#[derive(Debug)]
pub struct MatchArm {
    pub test: Box<Expr>,
    pub consequent: Box<Expr>,
}

#[derive(Debug)]
pub enum ExprKind {
    Number(f64),
    Str(Symbol),
    Ident(Symbol),
    Unary(Op, Box<Expr>),
    Binary {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>
    },
    Cond {
        test: Box<Expr>,
        consequent: Box<Expr>,
        alternate: Box<Expr>
    },
    Logical {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        arguments: Vec<Expr>,
    },
    Member {
        obj: Box<Expr>,
        property: Symbol,
    },
    Match {
        discriminant: Box<Expr>,
        cases: Vec<MatchArm>,
    }
}

#[derive(Debug)]
pub struct Stmt {
    kind: Box<StmtKind>
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Stmt {
            kind: Box::new(kind)
        }
    }
}

#[derive(Debug)]
pub enum StmtKind {
    LetDecl(Symbol, Box<Expr>),
}
