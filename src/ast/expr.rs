use crate::symbol::Symbol;
use crate::typecheck::TKind;
use id_arena::{Id};


#[derive(Debug)]
pub struct Expr {
    pub kind: Box<ExprKind>,
    /**
     * The internal type of the expression. For some expression kinds
     * we can determine this at parse time (specifically, literals like
     * numbers, strings, booleans, etc.)
     */
    pub ty: Option<TKind>,
}

pub type ExprId = Id<Expr>;

impl Expr {
    pub fn new(kind: ExprKind) -> Self {
        Expr {
            kind: Box::new(kind),
            ty: None,
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
    GreaterThan,
    LessThan,
}

#[derive(Debug)]
pub struct MatchArm {
    pub test: ExprId,
    pub consequent: ExprId,
}

#[derive(Debug)]
pub struct ObjectProperty(pub Symbol, pub ExprId);

#[derive(Debug)]
pub struct Call {
    pub callee: ExprId,
    pub arguments: Vec<ExprId>,
}

#[derive(Debug)]
pub enum ExprKind {
    Number(f64),
    Str(Symbol),
    Ident(Symbol),
    Bool(bool),
    Unary(Op, ExprId),
    Array(Vec<ExprId>),
    Object(Vec<ObjectProperty>),
    Binary {
        op: Op,
        left: ExprId,
        right: ExprId,
    },
    Cond {
        test: ExprId,
        consequent: ExprId,
        alternate: ExprId,
    },
    Logical {
        op: Op,
        left: ExprId,
        right: ExprId,
    },
    Call(Call),
    Member {
        obj: ExprId,
        property: Symbol,
    },
    Match {
        discriminant: ExprId,
        cases: Vec<MatchArm>,
    },
}

pub fn number_expr(num: f64) -> Expr {
    let mut expr = Expr::new(
        ExprKind::Number(num)
    );
    expr.ty = Some(TKind::Number);
    expr
}