use crate::symbol::Symbol;
use crate::typecheck::TKind;
use uuid::Uuid;

use super::expr::ExprId;

#[derive(Debug)]
pub struct Stmt {
    pub kind: Box<StmtKind>,
    pub id: Uuid,
}

impl Stmt {
    pub fn new(kind: StmtKind) -> Self {
        Stmt {
            kind: Box::new(kind),
            id: Uuid::new_v4(),
        }
    }
}

#[derive(Debug)]
pub struct Block(pub Vec<Stmt>);

#[derive(Debug)]
pub struct LetDecl {
    pub name: Symbol,
    pub init: ExprId,
}

#[derive(Debug)]
pub struct Param {
    pub name: Symbol,
    pub ty: TKind,
}

#[derive(Debug)]
pub enum StmtKind {
    LetDecl(LetDecl),
    Block(Block),
    If(ExprId, Block, Option<Block>),
    Return(ExprId),
}

#[derive(Debug)]
pub struct FuncDecl {
    pub name: Symbol,
    pub params: Option<Vec<Param>>,
    pub return_ty: Symbol,
    pub block: Block,
}

#[derive(Debug)]
pub enum DeclKind {
    Type,
    Component(FuncDecl),
    Func(FuncDecl),
}

#[derive(Debug)]
pub struct Decl {
    pub kind: DeclKind,
}
