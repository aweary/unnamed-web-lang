use super::ty_context::TyContext;
use syntax::ast::*;
// use syntax::symbol::Symbol;
use syntax::ty::{LiteralTy, Ty};
use syntax::Span;

pub type CheckResult<T> = std::result::Result<T, (String, Span)>;

fn check(ctx: &mut TyContext, expr: &Expr, ty: &Ty) -> CheckResult<()> {
    match (&expr.kind, ty) {
        // Check a literal against a literal type.
        (ExprKind::Lit(lit), Ty::Literal(_)) => {
            let inferred_ty = infer_lit(lit)?;
            if &inferred_ty == ty {
                // Literal type matches inference, good to go
                Ok(())
            } else {
                let msg = format!("Expected a {:?}, but got a {:?}", ty, inferred_ty);
                Err((msg, expr.span))
            }
        }
        (ExprKind::Reference(ident), _) => {
            // TODO handle reference errors here.
            let resolved_ty = ctx.resolve(ident).expect("Cant find ident");
            // TODO handle subtyping and all that.
            if ty == resolved_ty {
                Ok(())
            } else {
                let msg = format!("Expected a {:?}, but got a {:?}", ty, resolved_ty);
                Err((msg, expr.span))
            }
        }
        _ => Ok(()),
    }
}

pub fn infer(ctx: &mut TyContext, expr: &Expr) -> CheckResult<Ty> {
    match &expr.kind {
        ExprKind::Lit(lit) => infer_lit(lit),
        ExprKind::Call(ref fn_expr, ref args) => {
            let fn_ty = infer_fn(ctx, fn_expr)?;
            match fn_ty {
                // Inferred function type must be a function. Otherwise
                // we're trying to call a non-function value as a function,
                // which is an error.
                Ty::Function(ref input_ty, ref output_ty) => {
                    // TODO handle multiple arguments. We can represent
                    // those as tuple types if that makes it easier.
                    let arg = args.get(0).unwrap();
                    // Now we need to check that the funciton paramters match
                    // the expected type for the arguments.
                    check(ctx, arg, input_ty)?;
                    // Then we need to return the output type as the inferred
                    // type for this call expression.
                    Ok(*output_ty.clone())
                }
                _ => {
                    let msg = format!("Cannot call {:?} as a function", fn_ty);
                    Err((msg, expr.span))
                }
            }
        }
        ExprKind::Reference(ident) => {
            println!("reference");
            let ty = ctx.resolve(ident).expect("Reference error");
            Ok(ty.clone())
        }
        _ => Ok(Ty::Unit),
    }
}

/// Infer the type of an expression that we expect to resolve to
/// a function.
pub fn infer_fn(ctx: &mut TyContext, expr: &Expr) -> CheckResult<Ty> {
    // It should first resolve to a reference as we don't currently
    // support calling functions that are defined inline.
    // TODO support inline function definitions + calls.
    match &expr.kind {
        ExprKind::Reference(ident) => {
            // If we have the type for this symbol in context already,
            // we assume it is correct and return it.
            if let Some(ty) = ctx.resolve(ident) {
                Ok(ty.clone())
            } else {
                // If the type isn't in context, something wen't wrong.
                // We can't infer its type because we need to be able
                // to access the expression it refers to, and ctx
                // is responsible for that mapping.
                // This should never happen and indicates a type checker bug.
                panic!("Cant resolve a reference in typecheck");
            }
        }
        _ => Err(("Expected a function".into(), expr.span)),
    }
}

/// Infer the type of a literal value. This should just be a simple
/// 1:1 match between AST nodes and Ty kinds.
fn infer_lit(lit: &Lit) -> CheckResult<Ty> {
    let type_ = match lit.kind {
        LitKind::Number(_) => LiteralTy::Number,
        LitKind::Bool(_) => LiteralTy::Bool,
        LitKind::Str(_) => LiteralTy::String,
    };
    Ok(Ty::Literal(type_))
}
