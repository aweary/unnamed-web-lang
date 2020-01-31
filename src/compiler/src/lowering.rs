use crate::ctx::Context;
use crate::ir::*;
use diagnostics::{Diagnostic, FileId, Label, ParseResult as Result};
use parser::Parser;
use syntax::ast;

use fxhash::FxHashMap;

use std::path::PathBuf;

pub struct LoweringContext<'ctx> {
    ctx: &'ctx mut Context,
    file_id: FileId,
    base_path: &'ctx PathBuf,
    in_component: bool,
}

/// Takes a path, resolves it, parses the module, and attempts to lower it to the IR.
/// Uses caching to avoid lowering modules that are imported multiple times.
pub fn lower_module(ctx: &mut Context, path: &PathBuf) -> Result<ModuleId> {
    if let Some(module_id) = ctx.path_to_module_id.get(path) {
        Ok(*module_id)
    } else {
        // Check if we are in a cycle
        if ctx.import_path.contains(path) {
            // This path is already being lowered. This means we have a cyclical
            // import, which is *not* supported yet.
            // TODO Find out where the cycle started. Right now we report the entire import
            // path, starting from the beginning. The cycle could start anywhere.
            let (first_label_list, rest_labels) = ctx.import_path_nodes.split_at_mut(1);
            let mut first_label = first_label_list[0].clone();
            first_label.message = "This is where the cycle begins".into();
            let secondary_labels = rest_labels.to_vec();
            return Err(Diagnostic::new_error("Import cycle detected", first_label)
                .with_secondary_labels(secondary_labels));
        }
        // Add this path to the import path 'graph' so we can detect cycles
        ctx.import_path.push(path.clone());
        // Parse the imported module
        let (import_ast, file_id) = parse_module_from_path(ctx, path)?;
        let parent_path = &path.parent().unwrap().to_path_buf();
        // Lower it to IR so we can reference its locals
        let module_id = LoweringContext::new(ctx, file_id, parent_path).lower(import_ast)?;
        ctx.path_to_module_id.insert(path.clone(), module_id);
        Ok(module_id)
    }
}

// TODO dedupe this with compier.rs
fn parse_module_from_path(ctx: &mut Context, path: &PathBuf) -> Result<(syntax::ast::Mod, FileId)> {
    let file_id = ctx.add_file(path).unwrap();
    let source = ctx.resolve_file(file_id);
    // TODO maybe parser should just get the file_id and ctx reference?
    let mut parser = Parser::new(source, file_id);
    let ast = parser.parse_module()?;
    Ok((ast, file_id))
}

impl<'ctx> LoweringContext<'ctx> {
    pub fn new(ctx: &'ctx mut Context, file_id: FileId, base_path: &'ctx PathBuf) -> Self {
        LoweringContext {
            ctx,
            file_id,
            base_path,
            in_component: false,
        }
    }

    /// Lower a single module
    pub fn lower(&mut self, module: ast::Mod) -> Result<ModuleId> {
        let node_index = if let Some(node_index) = self.ctx.module_graph_node_map.get(&self.file_id)
        {
            *node_index
        } else {
            self.ctx.module_graph.add_node(self.file_id)
        };
        self.ctx
            .module_graph_node_map
            .insert(self.file_id, node_index);
        // We need to setup the scope for this module.
        self.ctx.scope_map.push_scope();
        let mut items = vec![];
        for item in module.items {
            let item = self.lower_item(item, ItemVisibility::Private)?;
            let id = self.ctx.item_arena.alloc(item);
            items.push(id);
        }
        let module = Module { items };
        // Tear down the scope for the module
        self.ctx.scope_map.pop_scope();
        let module_id = self.ctx.alloc_module(module);
        Ok(module_id)
    }

    fn lower_item(&mut self, item: ast::Item, visibility: ItemVisibility) -> Result<Item> {
        Ok(match item.kind {
            ast::ItemKind::Fn(fn_def) => {
                let fn_def_id = self.lower_fn(fn_def)?;
                let kind = ItemKind::Func(fn_def_id);
                Item { kind, visibility }
            }
            ast::ItemKind::Component(component_def) => {
                let component_def_id = self.lower_component(component_def)?;
                let kind = ItemKind::Component(component_def_id);
                Item { kind, visibility }
            }
            // When we encounter an import we need to follow the import path and
            // parse + lower the module if it hasn't already been processed.
            ast::ItemKind::Import(import) => {
                // Resolve the import path relative to the base path of the current module.
                // This is required because our current module system is path-based.
                let resolved_path = import.resolve(&self.base_path);

                // Add this to the graph of imports
                let label = Label::new(self.file_id, import.span, "");
                self.ctx.import_path_nodes.push(label);

                // Lower it to IR so we can reference its locals
                let module_id = lower_module(self.ctx, &resolved_path)?;
                let module = self.ctx.module_arena.get(module_id).unwrap();

                let mut item_name_to_id_map = FxHashMap::default();
                // Map of un-exported items, used for error reporting only.
                let mut private_item_name_id_map = FxHashMap::default();
                // Walk the top-level items and find the item that matches
                // the import specifier (if it exists)
                for item_id in &module.items {
                    let item = self.ctx.item_arena.get(*item_id).unwrap();
                    let name = item.name(&self.ctx);
                    if item.visibility != ItemVisibility::Public {
                        private_item_name_id_map.insert(name, item_id);
                        continue;
                    }
                    item_name_to_id_map.insert(name, item_id);
                }

                for specifier in import.specifiers {
                    let import_item_id = match item_name_to_id_map.get(&specifier.ident.name) {
                        Some(item_id) => *item_id,
                        None => {
                            let (title, label) =
                                if private_item_name_id_map.contains_key(&specifier.ident.name) {
                                    let title = "Attempted to import a private item";
                                    let label = format!(
                                        "'{:?}' is private and cannot be imported.",
                                        specifier.ident
                                    );
                                    (title, label)
                                } else {
                                    let title = "Unknown import";
                                    let label = format!(
                                        "No item called '{:?}' found in the imported module",
                                        specifier.ident,
                                    );
                                    (title, label)
                                };
                            // If we can't find the item in the module,
                            // this means we've imported an item that doesn't exist. Error.
                            return Err(Diagnostic::new_error(
                                title,
                                Label::new(self.file_id, specifier.span, label),
                            ));
                            // TODO spelling suggestions for typos
                        }
                    };
                    // Resolve the local variable name for this import
                    // TODO easier way to do this with Option methods?
                    let ident = if let Some(ident) = specifier.alias {
                        ident
                    } else {
                        specifier.ident
                    };

                    self.ctx.scope_map.define(
                        ident.name,
                        Reference {
                            span: ident.span,
                            kind: ReferenceKind::Item(*import_item_id),
                        },
                    );
                    // ...
                }
                // We need to add the import's local name to the scope of the
                // module. TODO
                let kind = ItemKind::Import;
                Item {
                    kind,
                    visibility: ItemVisibility::Private,
                }
            }
            ast::ItemKind::Export(item) => {
                // This just wraps an item and gives us new visibility info
                self.lower_item(*item, ItemVisibility::Public)?
            }
            
            _ => {
                todo!("Not implemented yet");
            }
        })
    }

    fn lower_component(&mut self, component_def: ast::ComponentDef) -> Result<ComponentDefId> {
        // Track that we're in a component, as some things are more restricted.
        self.in_component = true;
        let params = component_def.params;
        for param in params.into_iter() {
            self.lower_local_pattern(param.local, ReferenceKind::Param);
        }
        let body = *component_def.body;
        let block = self.lower_block(body)?;
        let def = ComponentDef {
            block,
            name: component_def.name,
        };
        self.in_component = false;
        Ok(self.ctx.component_arena.alloc(def))
    }

    // Currently only works with local patterns for params
    fn lower_local_pattern(&mut self, local: ast::LocalPattern, kind: ReferenceKind) {
        match local {
            ast::LocalPattern::Ident(ident, span) => {
                let reference = Reference {
                    // Is this fine? Do we need an ID at all? Maybe for
                    // type checking in the future...
                    // We do need to be able to check the param types
                    // for a function
                    kind,
                    span,
                };
                self.ctx.scope_map.define(ident.name, reference);
            }
            ast::LocalPattern::List(idents, _span) => {
                for ident in idents {
                    let reference = Reference {
                        kind: ReferenceKind::Param,
                        span: ident.span,
                    };
                    self.ctx.scope_map.define(ident.name, reference);
                }
                // ...
            }
            ast::LocalPattern::Object(properties, _span) => {
                for prop in properties {
                    // TODO need to also store `key`
                    self.lower_local_pattern(prop.value, kind.clone());
                }
            }
        };
    }

    fn lower_obj_pattern(&mut self, pattern: ast::LocalObjectProperty) {
        println!("pattern {:#?}", pattern);
        // ...
    }

    fn lower_template(&mut self, _template: ast::Template) -> Result<TemplateId> {
        let template = Template {};
        Ok(self.ctx.template_arena.alloc(template))
    }

    fn lower_fn(&mut self, fn_def: ast::FnDef) -> Result<FuncDefId> {
        // TODO need to iterate the params and add them to the local scope
        // of the function.
        let params = fn_def.params;
        for param in params.into_iter() {
            self.lower_local_pattern(param.local, ReferenceKind::Param);
        }
        let body = *fn_def.body;
        let block = self.lower_block(body)?;
        let def = FuncDef {
            block,
            name: fn_def.name,
        };
        Ok(self.ctx.func_def_arena.alloc(def))
    }

    fn lower_block(&mut self, block: ast::Block) -> Result<BlockId> {
        // Push a new scope for this block
        self.ctx.scope_map.push_scope();
        let mut stmts = vec![];
        for stmt in block.stmts {
            let stmt_id = self.lower_stmt(stmt)?;
            stmts.push(stmt_id);
        }
        self.ctx.scope_map.pop_scope();
        Ok(self.ctx.block_arena.alloc(Block { stmts }))
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> Result<StmtId> {
        let kind = match stmt.kind {
            ast::StmtKind::Expr(expr) => {
                let expr_id = self.lower_expr(*expr)?;
                StmtKind::Expr(expr_id)
            }
            ast::StmtKind::Local(local) => {
                let local_id = self.lower_local(*local)?;
                StmtKind::Local(local_id)
            }
            ast::StmtKind::Return(expr) => {
                let expr_id = self.lower_expr(*expr)?;
                StmtKind::Return(expr_id)
            }
            _ => {
                todo!();
            }
        };
        let stmt = Stmt { kind };
        Ok(self.ctx.stmt_arena.alloc(stmt))
    }

    fn lower_local(&mut self, local: ast::Local) -> Result<LocalId> {
        // Need to populate the scope map with the local(s) this definition
        // is introduction. We need to handle all potential local pattern cases
        // (destructuring).
        let init = if let Some(expr) = local.init {
            Some(self.lower_expr(*expr)?)
        } else {
            None
        };
        let ir_local = Local { init };
        let local_id = self.ctx.local_arena.alloc(ir_local);
        self.lower_local_pattern(local.name, ReferenceKind::Local(local_id));
        Ok(local_id)
    }

    fn lower_if_expr(&mut self, if_expr: ast::IfExpr) -> Result<IfExpr> {
        let condition = self.lower_expr(*if_expr.condition)?;
        let consequent = self.lower_block(*if_expr.block)?;
        let alt = if let Some(alt) = if_expr.alt {
            match alt {
                ast::Else::Block(block) => {
                    let block_id = self.lower_block(*block)?;
                    Some(IfExprAlt::Block(block_id))
                }
                ast::Else::If(expr) => {
                    // The parser technically allows any expression
                    // to follow, so we refine it here
                    // TODO fix parser types so we don't have to
                    match expr.kind {
                        ast::ExprKind::If(if_expr) => {
                            let if_expr = self.lower_if_expr(if_expr)?;
                            Some(IfExprAlt::If(Box::new(if_expr)))
                        }
                        _ => {
                            return Err(Diagnostic::new_error(
                                "Expected an if block",
                                Label::new(
                                    self.file_id,
                                    expr.span,
                                    "Arbitary expressions are not allowed after if blocks",
                                ),
                            ))
                        }
                    }
                }
            }
        } else {
            None
        };
        Ok(IfExpr {
            condition,
            consequent,
            alt,
        })
        // let kind = ExprKind::If {
        //     condition,
        //     consequent,
        // };
        // let expr = Expr { kind };
        // Ok(self.ctx.expr_arena.alloc(expr))
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> Result<ExprId> {
        match expr.kind {
            // A literal value (string, number, boolean)
            ast::ExprKind::Lit(lit) => {
                let lit = match lit.kind {
                    ast::LitKind::Bool(symbol) => Literal::Boolean(symbol),
                    ast::LitKind::Number(symbol) => Literal::Number(symbol),
                    ast::LitKind::Str(symbol) => Literal::String(symbol),
                };
                let expr = Expr {
                    kind: ExprKind::Literal(lit),
                };
                Ok(self.ctx.expr_arena.alloc(expr))
            }
            ast::ExprKind::If(if_expr) => {
                let if_expr = self.lower_if_expr(if_expr)?;
                let expr = Expr {
                    kind: ExprKind::If(if_expr),
                };
                Ok(self.ctx.expr_arena.alloc(expr))
            }
            // Binary expression
            ast::ExprKind::Binary(op, left, right) => {
                let left_id = self.lower_expr(*left)?;
                let right_id = self.lower_expr(*right)?;
                let expr = Expr {
                    kind: ExprKind::Binary(op, left_id, right_id),
                };
                Ok(self.ctx.expr_arena.alloc(expr))
            }
            // Call expression
            ast::ExprKind::Call(callee, args) => {
                let callee = self.lower_expr(*callee)?;
                let mut arguments = vec![];
                for arg in args {
                    let arg = self.lower_expr(arg)?;
                    arguments.push(arg);
                }
                let expr = Expr {
                    kind: ExprKind::Call { callee, arguments },
                };
                Ok(self.ctx.expr_arena.alloc(expr))
            }
            // Reference to some other named item. Resolve its type
            ast::ExprKind::Reference(reference) => {
                let name = reference.name;
                match self.ctx.scope_map.resolve(&name) {
                    Some(reference) => {
                        let expr = Expr {
                            kind: ExprKind::Reference(reference.to_owned()),
                        };
                        Ok(self.ctx.expr_arena.alloc(expr))
                        // ...
                    }
                    None => {
                        let mut secondary_labels = vec![];
                        let label_message = if let Some((name, reference)) =
                            self.ctx.scope_map.find_similar(&name)
                        {
                            secondary_labels.push(Label::new(
                                self.file_id,
                                reference.span,
                                format!("I found an item called '{:?}' here", name),
                            ));
                            format!("Unable to find name, did you mean '{:?}'?", name)
                        } else {
                            "Unable to find name".to_owned()
                        };
                        Err(Diagnostic::new_error(
                            "Unknown reference",
                            Label::new(self.file_id, expr.span, label_message),
                        )
                        .with_secondary_labels(secondary_labels))
                        // ...
                    }
                }
            }
            // Templates
            ast::ExprKind::Template(template) => {
                let template_id = self.lower_template(template)?;
                let expr = Expr {
                    kind: ExprKind::Template(template_id)
                };
                Ok(self.ctx.expr_arena.alloc(expr))
            }
            _ => {
                todo!();
            }
        }
    }
}
