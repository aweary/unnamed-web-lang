use super::ty_context::TyContext;
use syntax::ast::*;
// use syntax::symbol::Symbol;
use syntax::ty::{LiteralTy, Ty};
use syntax::Span;

use std::collections::HashSet;
use std::iter::FromIterator;

pub type CheckResult<T> = std::result::Result<T, (String, Span)>;

pub fn check(ctx: &mut TyContext, expr: &Expr, ty: &Ty) -> CheckResult<()> {
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

fn check_fn_app(
    ctx: &mut TyContext,
    input_ty: &Ty,
    args: &mut [Expr],
    call_expr: &Expr,
) -> CheckResult<()> {
    // TODO we don't need to do this here if we have an ArgsType enum that the parser
    // returns, like we do with ParamsType
    let args_ty = match args.len() {
        // No arguments, easy
        0 => Ty::Unit,
        // One argument
        1 => {
            let mut arg = args.get_mut(0).expect("checked");
            infer(ctx, &mut arg)?
        }
        _ => {
            let mut types = vec![];
            for arg in args.iter_mut() {
                let ty = infer(ctx, arg)?;
                types.push(ty);
            }
            Ty::Product(types)
        }
    };

    // Totally equal!
    if *input_ty == args_ty {
        Ok(())
    } else {
        match (&input_ty, &args_ty) {
            (Ty::Product(ref input_tys), Ty::Product(ref arg_tys)) => {
                for (i, (in_ty, arg_ty)) in input_tys.iter().zip(arg_tys.iter()).enumerate() {
                    if in_ty != arg_ty {
                        let expr = args.get(i).unwrap();
                        println!("expr {:?}", expr);
                        let msg = format!("Expected {:#?} but found {:#?}", in_ty, arg_ty);
                        return Err((msg, expr.span));
                    }
                }
                Ok(())
            }
            _ => {
                let msg = format!("\nExpected:\n{:#?}\n\nFound:\n{:#?}\n", input_ty, args_ty);
                Err((msg, call_expr.span))
            }
        }
    }
}

pub fn infer(ctx: &mut TyContext, expr: &mut Expr) -> CheckResult<Ty> {
    if let Some(ty) = &expr.ty {
        println!("Cached type {:?}", ty);
        return Ok(ty.clone());
    };
    let ty = match &mut expr.kind {
        // Template literal
        ExprKind::Template(_) => Ok(Ty::Template),
        // Monomorphic literal types (numbers, strings, booleans)
        ExprKind::Lit(lit) => infer_lit(lit),
        // Object literals
        ExprKind::Object(props) => {
            for (_, value) in props.iter_mut() {
                let value_ty = infer(ctx, value);
                println!("key {:?}", value_ty);
            }
            Ok(Ty::Unimplemented)
        }
        // Array literals
        ExprKind::Array(ref mut exprs) => {
            let mut types = vec![];
            for expr in exprs.iter_mut() {
                let ty = infer(ctx, expr)?;
                types.push(ty);
            }
            // Unique types.
            let ty_set: HashSet<Ty> = HashSet::from_iter(types.clone());
            let unique_tys = Vec::from_iter(ty_set.into_iter());
            let item_ty = match unique_tys.len() {
                // Don't know what the item type is
                0 => Ty::Existential,
                1 => unique_tys.get(0).expect("checked").clone(),
                _ => Ty::Union(unique_tys),
            };
            Ok(Ty::Array(item_ty.into()))
        }
        // Function expression
        ExprKind::Func(fn_def) => infer_fn(ctx, fn_def),
        // Function call
        ExprKind::Call(ref fn_expr, ref mut args) => {
            let fn_ty = infer_fn_return_ty(ctx, fn_expr)?;
            match fn_ty {
                // Inferred function type must be a function. Otherwise
                // we're trying to call a non-function value as a function,
                // which is an error.
                Ty::Function(ref input_ty, ref output_ty) => {
                    check_fn_app(ctx, input_ty, args, fn_expr)?;
                    // Now we need to check that the funciton paramters match
                    // the expected type for the arguments.
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
        _ => Ok(Ty::Unimplemented),
    }?;
    expr.ty = Some(ty.clone());
    Ok(ty)
}

pub fn infer_fn(
    ctx: &mut TyContext,
    FnDef {
        params, return_ty, ..
    }: &FnDef,
) -> CheckResult<Ty> {
    let input_ty = infer_fn_params(ctx, params)?;
    Ok(Ty::Function(input_ty.into(), return_ty.clone().into()))
}

/// Infers a product type from the list of params.
fn infer_fn_params(_ctx: &mut TyContext, params: &ParamType) -> CheckResult<Ty> {
    Ok(match params {
        ParamType::Empty => Ty::Unit,
        ParamType::Single(param) => param.ty.clone(),
        ParamType::Multi(ref params) => {
            let mut types = vec![];
            for param in params {
                let ty = param.ty.clone();
                types.push(ty);
            }
            Ty::Product(types)
        }
    })
}

/// Infer the type of an expression that we expect to resolve to
/// a function.
pub fn infer_fn_return_ty(ctx: &mut TyContext, expr: &Expr) -> CheckResult<Ty> {
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
