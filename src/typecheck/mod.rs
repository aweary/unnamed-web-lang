use crate::ast::*;
use crate::parser::ParsingContext;
use crate::symbol::Symbol;

// Built in terminal types
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TKind {
    // A number literal
    Number,
    // A string literal,
    String,
    // A boolean literal,
    Bool,
    // An HTML(ish) template,
    Template,
    // Unable to determine type
    Error,
}

// A union of multiple possible types
pub struct TUnion(Vec<TKind>);

// This maps the tokens representing the built-in types
// of the environment, to the internal representation
// the type system uses (TKind).
// If a new type is added, it needs to go here to be parsed.
pub fn type_of_built_in_symbol(ctx: &ParsingContext, sym: Symbol) -> TKind {
    match ctx.symbol_str(sym) {
        Some("int") | Some("number") => TKind::Number,
        Some("string") => TKind::String,
        Some("bool") => TKind::Bool,
        s => TKind::Error,
    }
}

pub fn infer(ctx: &ParsingContext, expr_id: ExprId) -> TKind {
    if ctx.has_ty_for(expr_id) {
        return ctx.type_of(expr_id);
    }
    let expr = ctx.resolve_expr(expr_id);
    match *expr.kind {
        ExprKind::Number(_) => TKind::Number,
        ExprKind::Str(_) => TKind::String,
        ExprKind::Bool(_) => TKind::Bool,
        ExprKind::Ident(sym) => ctx.resolve_type_of(sym),
        ExprKind::Template(_) => TKind::Template,
        // TODO handle non-numeric binary expressions
        ExprKind::Binary { left, right, .. } => {
            let left_ty = infer(&ctx, left);
            let right_ty = infer(&ctx, right);
            if left_ty == right_ty {
                left_ty
            } else {
                TKind::Error
            }
        }
        _ => TKind::Error,
    }
}

// pub fn check(expr: ExprId, kind: TKind) -> bool {
//   // ...
// }

// pub fn union(a: TKind, b: TKind) -> TUnion {
//   TUnion(vec![a, b])
// }
