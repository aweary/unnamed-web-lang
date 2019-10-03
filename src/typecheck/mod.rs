use crate::ir::Expr;

// Built in terminal types
pub enum TKind {
  // A number literal
  Number,
  // A string literal,
  String,
  // A boolean literal,
  Bool,
  // A polymorphic array type
  Array(Box<Type>),
}

// A union of multiple possible types
pub struct TUnion(Vec<TKind>);

pub fn infer(expr: &Expr) -> TKind {
  // ...
}

pub fn check(expr: &Expr, kind: TKind) -> bool {
  // ...
}

pub fn union(a: TKind, b: TKind) -> TUnion {
  TUnion(vec![a, b])
}