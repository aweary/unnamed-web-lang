use crate::ast::*;
use crate::parser::ParsingContext;

// Built in terminal types
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TKind {
  // A number literal
  Number,
  // A string literal,
  String,
  // A boolean literal,
  Bool,
  // Unable to determine type
  Error,
}

// A union of multiple possible types
pub struct TUnion(Vec<TKind>);

pub fn infer(ctx: &ParsingContext, expr_id: ExprId) -> TKind {
  if ctx.has_ty_for(expr_id) {
    return ctx.type_of(expr_id)
  }
  let expr = ctx.resolve_expr(expr_id);
  match *expr.kind {
    ExprKind::Number(_) => TKind::Number,
    ExprKind::Str(_) => TKind::String,
    ExprKind::Bool(_) => TKind::Bool,
    ExprKind::Ident(sym) => {
      ctx.resolve_type_of(sym)
    },
    // TODO handle non-numeric binary expressions
    ExprKind::Binary { left, right, ref op } => {
      let left_ty = infer(&ctx, left);
      let right_ty = infer(&ctx, right);
      if left_ty == right_ty {
        left_ty
      } else {
        TKind::Error
      }
    }
    _ => TKind::Error
  }
}

// pub fn check(expr: ExprId, kind: TKind) -> bool {
//   // ...
// }

// pub fn union(a: TKind, b: TKind) -> TUnion {
//   TUnion(vec![a, b])
// }