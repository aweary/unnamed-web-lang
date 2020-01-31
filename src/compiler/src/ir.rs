use id_arena::Id;
use crate::ctx::Context;
use syntax::{Span, symbol::Symbol};

// Things we reuse from the AST
pub use syntax::ast::{BinOp, Ident};

/// Uniquely identifies a module in the module graph IR
pub type ModuleId = Id<Module>;

/// Uniquely identifies a top-level item in a module
pub type ItemId = Id<Item>;

/// An individual module. The module system currently only allows
/// you to define a new module by creating a new file, so this maps
/// 1:1 to a file.
#[derive(Debug)]
pub struct Module {
    pub items: Vec<ItemId>,
}

/// A top-level item contained by a module. 
#[derive(Debug)]
pub struct Item {
    /// The type of item
    pub kind: ItemKind,
    /// The visibility of the item, i.e., is it exported from the module?
    pub visibility: ItemVisibility,
}

impl Item {
    pub fn name(&self, ctx: &Context) -> Symbol {
        match &self.kind {
            ItemKind::Func(func_def_id) => {
                let func_def = &ctx.func_def_arena[*func_def_id];
                func_def.name.name.clone()
            }
            _ => Symbol::intern("IDK"),
        }
    }
}

#[derive(Debug)]
pub enum ItemKind {
    /// An import from another module
    Import,
    /// A function definition
    Func(FuncDefId),
    /// A component definition
    Component(ComponentDefId),
    /// An enum definition
    Enum(EnumDefId),
    /// A type definition
    Type(TypeDefId),
}

#[derive(Debug, PartialEq, Eq)]
pub enum ItemVisibility {
    Private,
    Public,
}

/// A function definition
#[derive(Debug)]
pub struct FuncDef {
    pub name: Ident,
    pub block: BlockId,
}

/// Unique ID pointing to a function definition
pub type FuncDefId = Id<FuncDef>;

pub struct ComponentDef {
    pub name: Ident,
    pub block: BlockId,
}

pub type ComponentDefId = Id<ComponentDef>;


/// A block of code, denoted by a pair of curly braces in the source code
#[derive(Debug)]
pub struct Block {
    pub stmts: Vec<StmtId>,
}

/// Unique ID pointing to a block
pub type BlockId = Id<Block>;

#[derive(Debug)]
pub struct Stmt {
    pub kind: StmtKind
}

#[derive(Debug)]
pub enum StmtKind {
    Expr(ExprId),
    Local(LocalId),
    Return(ExprId),
}

#[derive(Debug)]
pub struct Local {
    pub init: Option<ExprId>,
}

pub type LocalId = Id<Local>;

/// Unique ID pointing to a stmt
pub type StmtId = Id<Stmt>;

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug)]
pub enum ExprKind {
    /// Reference to some named value
    Reference(Reference),
    /// Inline literal value
    Literal(Literal),
    /// Binary expression
    Binary(BinOp, ExprId, ExprId),
    /// Call expression
    Call { callee: ExprId, arguments: Vec<ExprId> },
    /// If expression
    If(IfExpr),
    /// Template expression
    Template(TemplateId),
}

#[derive(Debug)]
pub enum IfExprAlt {
    Block(BlockId),
    If(Box<IfExpr>),
}

#[derive(Debug)]
pub struct IfExpr {
    pub condition: ExprId,
    pub consequent: BlockId,
    pub alt: Option<IfExprAlt> 
}


#[derive(Debug)]
pub enum Literal {
    Number(Symbol),
    String(Symbol),
    Boolean(Symbol),
}


#[derive(Debug, Clone)]
pub struct Reference {
    pub kind: ReferenceKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ReferenceKind {
    Item(ItemId),
    Local(LocalId),
    // TODO
    Param,
}


#[derive(Debug, Clone)]
pub struct Template {}

pub type TemplateId = Id<Template>;

/// Unique ID pointing to an expression
pub type ExprId = Id<Expr>;

/// An enum definition
#[derive(Debug)]
pub struct EnumDef {}

/// Unique ID pointing to an enum definition
pub type EnumDefId = Id<EnumDef>;

/// A type definition
#[derive(Debug)]
pub struct TypeDef {}

/// Unique ID pointing to an enum definition
pub type TypeDefId = Id<TypeDef>;