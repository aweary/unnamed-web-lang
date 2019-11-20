use crate::ast::*;

pub trait Visitor: Sized {
    fn visit_item(&mut self, item: &mut Item) {
        walk_item(self, item);
        // Item...
    }

    fn visit_fn_decl(&mut self, decl: &mut FnDecl) {
        walk_fn_decl(self, decl);
    }

    fn visit_stmt(&mut self, stmt: &mut Stmt) {
        use StmtKind::*;
        match stmt.kind {
            Expr(ref mut expr) => self.visit_expr(expr),
            Local(ref mut local) => self.visit_local(local),
            Item(ref mut item) => self.visit_item(item),
            While(ref mut expr, ref mut block) => {
                // TODO visit_while
                self.visit_expr(expr);
                self.visit_block(block);
            }
            Return(ref mut expr) => self.visit_expr(expr),
            _ => (),
        };
        // ...
    }

    fn visit_expr(&mut self, expr: &mut Expr) {
        walk_expr(self, expr);
        //
    }

    fn visit_local(&mut self, local: &mut Local) {
        walk_local(self, local);
    }

    fn visit_reference(&mut self, _ident: &mut Ident) {
        // ...
    }

    fn visit_param(&mut self, Param { ref mut local, .. }: &mut Param) {
        walk_local_pattern(self, local);
        // ...
    }

    fn visit_ident(&mut self, _ident: &mut Ident) {}

    fn visit_block(&mut self, block: &mut Block) {
        walk_block(self, block);
    }

    fn visit_mod(&mut self, module: &mut Mod) {
        walk_mod(self, module);
    }

    fn visit_program(&mut self, program: &mut Program) {
        walk_program(self, program)
    }
    // ...
}

pub fn walk_program<V: Visitor>(visitor: &mut V, program: &mut Program) {
    for module in program.modules.iter_mut() {
        visitor.visit_mod(module)
    }
}

pub fn walk_mod<V: Visitor>(visitor: &mut V, module: &mut Mod) {
    for item in module.items.iter_mut() {
        visitor.visit_item(item)
    }
}

pub fn walk_expr<V: Visitor>(visitor: &mut V, Expr { kind, .. }: &mut Expr) {
    use ExprKind::*;
    match kind {
        If(ref mut condition, ref mut block, ref mut alt_block) => {
            visitor.visit_expr(condition);
            visitor.visit_block(block);
            if let Some(ref mut block) = alt_block {
                visitor.visit_block(block);
            }
        }
        For(ref mut pattern, ref mut expr, ref mut block) => {
            walk_local_pattern(visitor, pattern);
            visitor.visit_expr(expr);
            visitor.visit_block(block);
        }
        Binary(_op, ref mut left, ref mut right) => {
            visitor.visit_expr(left);
            visitor.visit_expr(right);
        }
        Block(ref mut block) => {
            visitor.visit_block(block);
        }
        Reference(ref mut ident) => {
            visitor.visit_reference(ident);
        }
        Call(ref mut fn_ref, ref mut arguments) => {
            visitor.visit_expr(fn_ref);
            for arg in arguments {
                visitor.visit_expr(arg);
            }
        }
        Array(ref mut exprs) => {
            for expr in exprs {
                visitor.visit_expr(expr);
            }
        }
        _ => (),
    };
}

pub fn walk_block<V: Visitor>(visitor: &mut V, block: &mut Block) {
    for stmt in block.stmts.iter_mut() {
        visitor.visit_stmt(stmt)
    }
}

pub fn walk_fn_decl<V: Visitor>(visitor: &mut V, FnDecl { params, .. }: &mut FnDecl) {
    for param in params.iter_mut() {
        visitor.visit_param(param);
        // ...
    }
    // ...
}

pub fn walk_local_object_property<V: Visitor>(visitor: &mut V, property: &mut LocalObjectProperty) {
    use LocalPattern::*;
    match property.value {
        Ident(ref mut ident, _) => {
            visitor.visit_ident(ident);
        }
        Object(ref mut properties, _) => {
            for property in properties.iter_mut() {
                walk_local_object_property(visitor, property);
            }
        }
        List(ref mut idents, _) => {
            for ident in idents.iter_mut() {
                visitor.visit_ident(ident);
            }
        }
    };
}

pub fn walk_local_pattern<V: Visitor>(visitor: &mut V, pattern: &mut LocalPattern) {
    use LocalPattern::*;
    match pattern {
        Ident(ref mut ident, _) => {
            visitor.visit_ident(ident);
        }
        Object(ref mut properties, _) => {
            for property in properties.iter_mut() {
                walk_local_object_property(visitor, property);
            }
            // ...
        }
        List(idents, _) => {
            for ident in idents.iter_mut() {
                visitor.visit_ident(ident);
            }
            // ...
        }
    }
    // ...
}

pub fn walk_item<V: Visitor>(visitor: &mut V, item: &mut Item) {
    use ItemKind::*;
    visitor.visit_ident(&mut item.ident);
    match item.kind {
        Enum(ref _enum_def, ref _generics) => {
            // ...
        }
        Fn(ref mut decl, ref _header, ref _genrics, ref mut block) => {
            visitor.visit_fn_decl(decl);
            visitor.visit_block(block);
            // ...
        }
        _ => {
            // ...
        }
    }
}

pub fn walk_local<V: Visitor>(
    visitor: &mut V,
    Local {
        init, ref mut name, ..
    }: &mut Local,
) {
    if let Some(ref mut init) = init {
        visitor.visit_expr(init)
    };
    walk_local_pattern(visitor, name);
}