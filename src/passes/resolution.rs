use crate::ast::*;
use crate::parser::ParsingContext;
use crate::symbol::Symbol;
use crate::visitor::{walk_block, walk_expr, walk_fn, Visitor};
use std::collections::VecDeque;

pub struct NameResolutionPass<'a> {
    param_queue: VecDeque<&'a Param>,
}

impl<'a> NameResolutionPass<'a> {
    pub fn new() -> Self {
        Self {
            param_queue: VecDeque::new(),
        }
    }

    pub fn populated_symbol_table(&mut self, ctx: &ParsingContext, module: &'a Module) {
        self.visit_mod(&ctx, &module);
    }
}

impl<'ast> Visitor<'ast> for NameResolutionPass<'ast> {

    fn visit_block(&mut self, ctx: &ParsingContext, block: &'ast Block) {
        ctx.begin_scope();
        for param in &self.param_queue {
            ctx.define_abstract(param.name, param.ty)
        }
        self.param_queue.clear();
        walk_block(self, &ctx, &block);
        ctx.end_scope();
    }

    fn visit_let(&mut self, ctx: &ParsingContext, decl: &'ast LetDecl) {
        let expr_id = decl.init;
        let name = decl.name;
        let ty = ctx.type_of(expr_id);
        ctx.define_local(name, expr_id);
        println!("{:?} has type : {:?}", name, ty);
        walk_expr(self, &ctx, decl.init);
    }

    fn visit_ident(&mut self, ctx: &ParsingContext, ident: &Symbol) {
        if !ctx.is_defined(ident) {
            println!("UNKNOWN IDENTIFIER: {:?}", ident);
        }
    }

    /**
     * Functions require special handling as their params define
     * values that are scoped to the function's block. Its the one
     * case where we define values scoped to a block *outside* that
     * block.
     */
    fn visit_fn(&mut self, ctx: &ParsingContext, func: &'ast FuncDecl) {
        //   self.symbols.define(func.name, );
        match &func.params {
            Some(param_list) => {
                for param in param_list {
                    self.param_queue.push_front(&param);
                }
            }
            None => (),
        };
        walk_fn(self, &ctx, &func);
    }
}
