use data_structures::control_flow_graph::{Block, Blockable, ControlFlowEdge, ControlFlowGraph};
use data_structures::scope_map::ScopeMap;
use diagnostics::ParseResult as Result;
// use diagnostics::
use codespan_reporting::diagnostic::{Diagnostic, Label};
use hir;
use session::Session;
use source::FileId;
use std::sync::Arc;
use syntax::ast;
use syntax::symbol::Symbol;

// hmmm

#[derive(Debug, Clone)]
struct CFGStatement(pub Arc<hir::Statement>);

impl Blockable for CFGStatement {
    fn has_early_exit(&self) -> bool {
        self.0.has_early_exit()
    }
}

pub struct LoweringContext<'sess> {
    scope: ScopeMap<Symbol, hir::Binding>,
    sess: &'sess mut Session,
    ignore_next_scope: bool,
    current_cfg: Option<ControlFlowGraph<CFGStatement>>,
    // TODO move these into a shared context so we can share arenas
    // across modules.
    file: FileId,
}

impl<'sess> LoweringContext<'sess> {
    pub fn new(sess: &'sess mut Session, file: FileId) -> Self {
        LoweringContext {
            sess,
            file,
            ignore_next_scope: false,
            current_cfg: None,
            scope: Default::default(),
        }
    }

    pub fn lower_module(&mut self, module: ast::Module) -> Result<hir::Module> {
        // Define the module-level scope
        self.scope.enter_scope();
        let mut definitions = vec![];
        for item in module.items {
            match item.kind {
                ast::ItemKind::Fn(fndef) => {
                    let fn_def = self.lower_fn(fndef)?;
                    let def = hir::Definition {
                        kind: hir::DefinitionKind::Function(fn_def),
                        visibility: hir::DefinitionVisibility::Private,
                    };
                    definitions.push(def);
                }
                ast::ItemKind::Component(compdef) => {
                    let compdef = self.lower_component(compdef)?;
                    let def = hir::Definition {
                        kind: hir::DefinitionKind::Component(compdef),
                        visibility: hir::DefinitionVisibility::Private,
                    };
                    definitions.push(def);
                }
                _ => unimplemented!(),
            };
        }
        self.scope.exit_scope();
        let module = hir::Module { definitions };
        Ok(module)
    }

    fn lower_fn_params(&mut self, params: ast::ParamType) -> Result<Vec<Arc<hir::Param>>> {
        let mut hir_params = vec![];
        for param in params {
            let name = param.name();
            let param = Arc::new(hir::Param {
                local: param.local,
                span: param.span,
                ty: param.ty,
            });
            self.scope
                .define(name, hir::Binding::Argument(param.clone()));
            hir_params.push(param);
        }
        Ok(hir_params)
    }

    /// Currently identical to lower_fn, except it returns hir::Component instead of hir::Function.
    /// This separate code path will let us add more constraints for components in the future.
    fn lower_component(&mut self, compdef: ast::ComponentDef) -> Result<Arc<hir::Component>> {
        // TODO figure out how to fill this out
        let cfg = ControlFlowGraph::default();
        let name = compdef.name.name.clone();
        // Push a new scope before we lower the block, so the params are included
        self.ignore_next_scope = true;
        self.scope.enter_scope();
        let params = self.lower_fn_params(compdef.params)?;
        let block = self.lower_block(*compdef.body)?;
        self.scope.exit_scope();
        self.ignore_next_scope = false;
        let compdef = Arc::new(hir::Component {
            params,
            name: compdef.name,
            span: compdef.span,
            graph: cfg,
            body: block,
        });
        self.scope
            .define(name, hir::Binding::Component(compdef.clone()));
        Ok(compdef)
    }

    fn lower_fn(&mut self, fndef: ast::FnDef) -> Result<Arc<hir::Function>> {
        // TODO figure out how to fill this out
        let cfg = ControlFlowGraph::default();
        let name = fndef.name.name.clone();
        // Push a new scope before we lower the block, so the params are included
        self.ignore_next_scope = true;
        self.scope.enter_scope();
        let params = self.lower_fn_params(fndef.params)?;
        let block = self.lower_block(*fndef.body)?;
        self.scope.exit_scope();
        let fndef = Arc::new(hir::Function {
            params,
            name: fndef.name,
            span: fndef.span,
            graph: cfg,
            body: block,
        });
        self.scope
            .define(name, hir::Binding::Function(fndef.clone()));
        Ok(fndef)
    }

    // fn lower_fn_cfg(&mut self, fn_def: ast::FnDef) {
    //     // The control flow graph for this function
    //     let mut cfg = ControlFlowGraph::default();
    //     // Initial function definition
    //     let fn_body_graph = self.lower_block_cfg(*fn_def.body, &mut cfg);
    //     // Add an edge from the last item to the exit node
    //     if let Some(fn_body_graph_exit_node) = fn_body_graph.last() {
    //         if !cfg.block_has_early_exit(*fn_body_graph_exit_node) {
    //             cfg.add_edge_to_exit(*fn_body_graph_exit_node, ControlFlowEdge::Normal);
    //         }
    //     }
    //     // Any queued edges will point to the exit node, as we're done with the graph
    //     cfg.flush_edge_queue_to_exit_block();
    //     // println!("\n{:?}", fn_def.name);
    //     cfg.print();
    // }

    fn lower_block(&mut self, block: ast::Block) -> Result<hir::Block> {
        // TODO move this out
        let mut cfg = ControlFlowGraph::default();
        let mut cfg_block = Block::default();
        let mut block_indicies = vec![];
        if !self.ignore_next_scope {
            self.scope.enter_scope();
        }
        let mut statements = vec![];
        for stmt in block.stmts {
            let stmt = self.lower_stmt(stmt)?;
            let stmt = Arc::new(stmt);
            // Construct the CFG for the block, following this new statement
            match stmt.kind {
                hir::StatementKind::Return(_) => {
                    // Add to the basic block
                    cfg_block.push(CFGStatement(stmt.clone()));
                    // Finalize the block and add it to the CFG
                    let block_index = cfg.add_block(cfg_block);
                    block_indicies.push(block_index);
                    // We know this block will only have an edge to the exit node, so add it here
                    cfg.add_edge_to_exit(block_index, ControlFlowEdge::Return);
                    // Create a new block, preempting other statements
                    cfg_block = Block::default();
                }
                _ => {
                    cfg_block.push(CFGStatement(stmt.clone()));
                    // ...
                }
            };
            statements.push(stmt);
        }
        if !self.ignore_next_scope {
            self.scope.exit_scope();
        }
        Ok(hir::Block { statements })
    }

    fn lower_expr(&mut self, expr: ast::Expr) -> Result<hir::Expr> {
        use ast::ExprKind;
        let kind = match expr.kind {
            ExprKind::Array(exprs) => {
                // Doing it this way so I can use ? on lower_expr
                let mut hir_exprs = vec![];
                for expr in exprs {
                    hir_exprs.push(self.lower_expr(expr)?);
                }
                hir::ExprKind::Array(hir_exprs)
            }
            ExprKind::Object(props) => {
                let mut hir_props = vec![];
                for (key, value) in props {
                    let value = self.lower_expr(value)?;
                    hir_props.push((key, value));
                    // ...
                }
                hir::ExprKind::Object(hir_props)
            }
            ExprKind::Tuple(_) => {
                unimplemented!("Tuple expression not implemented");
            }
            // Block expression
            ExprKind::Block(_) => {
                panic!("Block expressions are temporarily unsupported");
            }
            // A binary operation
            ExprKind::Binary(op, left, right) => hir::ExprKind::Binary(
                op,
                Box::new(self.lower_expr(*left)?),
                Box::new(self.lower_expr(*right)?),
            ),
            // A unary operation
            ExprKind::Unary(op, unexpr) => {
                hir::ExprKind::Unary(op, Box::new(self.lower_expr(*unexpr)?))
            }
            // Conditional expression, e.g., ternary
            ExprKind::Cond(_, _, _) => {
                unimplemented!("Cond expression not implemented");
            }
            // Call expression
            ExprKind::Call(expr, args) => match self.lower_expr(*expr)?.kind {
                hir::ExprKind::Reference(binding) => {
                    let mut hir_args = vec![];
                    for arg in args {
                        hir_args.push(self.lower_expr(arg)?);
                    }
                    hir::ExprKind::Call(binding, hir_args)
                }
                _ => {
                    unimplemented!("Can't call computed functions yet");
                }
            },
            // Assignment expression
            // TODO the left hand side should be a LeftExpr or something
            ExprKind::Assign(op, reference, value) => {
                if let ast::ExprKind::Reference(reference) = reference.kind {
                    match self.scope.resolve(&reference.name) {
                        Some(binding) => match binding {
                            hir::Binding::Local(local) => {
                                let value = self.lower_expr(*value)?;
                                hir::ExprKind::Assign(op, local, Box::new(value))
                            }
                            // This is a special variant of assign expressions, which
                            // are updating a state value.
                            hir::Binding::State(local) => {
                                let value = self.lower_expr(*value)?;
                                hir::ExprKind::StateUpdate(op, local, Box::new(value))
                            }
                            _ => {
                                panic!("Cant reassign functions or types!");
                            }
                        },
                        None => {
                            panic!("Reference error!");
                            // Reference error!
                        }
                    }
                } else {
                    panic!("No assignment to non references!");
                    // You can't assign to anything other than a reference for now.
                    // Object/array assignments come later.
                }
            }
            // Object member access
            ExprKind::Member(_, _) => {
                unimplemented!("Member expression not implemented");
            }
            // Object optional member access,
            ExprKind::OptionalMember(_, _) => {
                unimplemented!("OptionalMember expression not implemented");
            }
            // A literal
            ExprKind::Lit(lit) => hir::ExprKind::Lit(lit),
            // A variable reference
            ExprKind::Reference(ident) => {
                match self.scope.resolve(&ident.name) {
                    Some(binding) => hir::ExprKind::Reference(binding),
                    None => {
                        // This is where we handle reference errors.
                        // TODO move this logic out into some unified error reporting interface
                        let message = "Unknown reference";
                        let label = "Cannot find any value with this name";
                        return Err(Diagnostic::new_error(
                            message,
                            Label::new(self.file, ident.span, label),
                        ));
                    }
                }
            }
            // An `if` block with optional `else` block
            ExprKind::If(_) => {
                unimplemented!("If expression not implemented");
            }
            // For expression
            ExprKind::For(_, _, _) => {
                unimplemented!("For expression not implemented");
            }
            // An index operation
            ExprKind::Index(_, _) => {
                unimplemented!("Index expression not implemented");
            }
            // A `return` with an optional return expression
            ExprKind::Return(_) => {
                unimplemented!("Return expression not implemented");
            }
            // Template
            ExprKind::Template(template) => {
                let template = self.lower_template(template)?;
                hir::ExprKind::Template(template)
            }
            // Match
            ExprKind::Match(_, _) => {
                unimplemented!("Match expression not implemented");
            }
            // Function expression
            ExprKind::Func(fndef) => {
                let fndef = self.lower_fn(*fndef)?;
                hir::ExprKind::Func(fndef)
            }
            // GraphQL query
            ExprKind::Query(_) => {
                unimplemented!("Query expression not implemented");
            }
        };
        Ok(hir::Expr {
            span: expr.span,
            ty: expr.ty,
            kind,
        })
    }

    fn lower_template(&mut self, template: ast::Template) -> Result<hir::Template> {
        let mut instrs = vec![];
        // Assume the template is static, until we hit some dynamic content
        let mut kind = hir::TemplateKind::Static;
        let open = template.open;
        let ident = open.name;

        // We currently use the same JSX herustic of uppercase denoting components
        // but that will probably change soon
        match &ident.name.as_str().chars().nth(0).unwrap() {
            'A'..='Z' => {
                match self.scope.resolve(&ident.name) {
                    Some(binding) => {
                        match binding {
                            hir::Binding::Component(component) => {
                                instrs.push(hir::TemplateInstr::OpenCustomElement(component))
                            }
                            _ => {
                                // This is where we handle reference errors.
                                // TODO move this logic out into some unified error reporting interface
                                let message = "Cannot use this value as a component";
                                let label = "Functions and components are not the same";
                                return Err(Diagnostic::new_error(
                                    message,
                                    Label::new(self.file, ident.span, label),
                                ));
                            }
                        }
                        println!("binding!");
                    }
                    None => {
                        // This is where we handle reference errors.
                        // TODO move this logic out into some unified error reporting interface
                        let message = "Unknown reference";
                        let label = "Cannot find any value with this name";
                        return Err(Diagnostic::new_error(
                            message,
                            Label::new(self.file, ident.span, label),
                        ));
                    }
                }
            }
            _ => {
                // Open the element
                instrs.push(hir::TemplateInstr::OpenElement(ident.name));
            }
        };
        // Add any attributes
        for attr in open.attrs {
            let value = self.lower_expr(attr.value)?;
            match value.kind {
                hir::ExprKind::Lit(lit) => {
                    instrs.push(hir::TemplateInstr::SetStaticAttribute(attr.name.name, lit));
                    // ...
                }
                _ => {
                    kind = hir::TemplateKind::Dynamic;
                    instrs.push(hir::TemplateInstr::SetAttribute(attr.name.name, value))
                }
            }
        }

        if let Some(children) = template.children {
            use ast::TemplateChild;
            for child in children {
                match child {
                    TemplateChild::Expr(expr) => {
                        kind = hir::TemplateKind::Dynamic;
                        let expr = self.lower_expr(*expr)?;
                        instrs.push(hir::TemplateInstr::EmbedExpression(expr));
                    }
                    TemplateChild::Text(text) => {
                        instrs.push(hir::TemplateInstr::EmbedText(text));
                    }
                    TemplateChild::Template(template) => {
                        let template = self.lower_template(*template)?;
                        if template.kind == hir::TemplateKind::Dynamic {
                            kind = template.kind;
                        }
                        instrs.extend(template.instrs);
                        // ...
                    }
                }
            }
        }
        instrs.push(hir::TemplateInstr::CloseElement);
        Ok(hir::Template { instrs, kind })
    }

    fn lower_local(&mut self, local: ast::Local) -> Result<Arc<hir::Local>> {
        let name: Symbol = local.name.clone().into();
        // Avoiding .map so the ? propagates errors
        let init = if let Some(expr) = local.init {
            Some(Box::new(self.lower_expr(*expr)?))
        } else {
            None
        };
        let local = hir::Local {
            name: local.name,
            span: local.span,
            ty: local.ty,
            init,
        };
        let local = Arc::new(local);
        Ok(local)
    }

    fn lower_if(&mut self, ifexpr: ast::IfExpr) -> Result<hir::IfExpr> {
        let ast::IfExpr {
            condition,
            block,
            alt,
            span,
        } = ifexpr;
        let condition = self.lower_expr(*condition)?;
        let block = self.lower_block(*block)?;
        let alt = if let Some(alt) = alt {
            match alt {
                ast::Else::Block(block) => {
                    let block = self.lower_block(*block)?;
                    Some(hir::Else::Block(Box::new(block)))
                }
                ast::Else::If(ifexpr) => {
                    let ifexpr = self.lower_if(*ifexpr)?;
                    Some(hir::Else::If(Box::new(ifexpr)))
                }
            }
        } else {
            None
        };
        Ok(hir::IfExpr {
            condition: Box::new(condition),
            block: Box::new(block),
            span,
            alt,
        })
    }

    fn lower_stmt(&mut self, stmt: ast::Stmt) -> Result<hir::Statement> {
        use ast::StmtKind;
        let kind = match stmt.kind {
            // A local, let binding
            StmtKind::Local(local) => {
                let local = self.lower_local(*local)?;
                let name: Symbol = local.name.clone().into();
                self.scope.define(name, hir::Binding::Local(local.clone()));
                hir::StatementKind::Local(local)
            }
            StmtKind::State(local) => {
                let local = self.lower_local(*local)?;
                let name: Symbol = local.name.clone().into();
                self.scope.define(name, hir::Binding::State(local.clone()));
                hir::StatementKind::State(local)
            }
            // An item definition, local to some block
            StmtKind::Item(_) => {
                unimplemented!("Item statement not implemented");
            }
            // Expression statement
            // TODO should we differentiate expressions with or without semicolons?
            StmtKind::Expr(expr) => {
                let expr = self.lower_expr(*expr)?;
                hir::StatementKind::Expr(expr)
            }
            // A while loop
            StmtKind::While(_, _) => {
                unimplemented!("While statement not implemented");
            }
            // If statement
            StmtKind::If(ifexpr) => {
                let ifexpr = self.lower_if(ifexpr)?;
                hir::StatementKind::If(ifexpr)
            }
            // Return statement
            StmtKind::Return(expr) => {
                let expr = self.lower_expr(*expr)?;
                hir::StatementKind::Return(expr)
            }
            // Try/catch statement
            StmtKind::TryCatch(_, _, _) => {
                unimplemented!("TryCatch statement not implemented");
            }
        };
        Ok(hir::Statement {
            span: stmt.span,
            kind,
        })
    }

    /// Lower a block of code
    fn lower_block_cfg(&mut self, _body: ast::Block, _cfg: &mut ControlFlowGraph<hir::Statement>) {
        // // Start off by creating a basic block. It's possible this block might be empty,
        // // if this syntactic block is empty.
        // let mut block_indicies = vec![];
        // let mut block = Block::default();
        // for stmt in body.stmts {
        //     use ast::StmtKind::*;
        //     match stmt.kind {
        //         // Local(ref local) => {
        //         //     let name: Symbol = local.name.clone().into();
        //         //     self.scope.define(name, hir::Binding::Local);
        //         //     let stmt = hir::Statement {
        //         //         kind: hir::StatementKind::LocalDefinition,
        //         //         span: stmt.span,
        //         //     };
        //         //     // Add it to the current basic block
        //         //     block.push(stmt);
        //         // }
        //         If(ref if_expr) => {
        //             let branching_block = block;
        //             block = Block::default();
        //             let (if_branch_indicies, partial_edges) =
        //                 self.control_flow_graph_for_if_statement(if_expr, cfg, branching_block);
        //             block_indicies.extend(if_branch_indicies.iter());
        //             for PartialFromEdge(block_index, edge) in partial_edges {
        //                 cfg.enqueue_edge(block_index, edge);
        //                 // ...
        //             }
        //         }
        //         Return(expr) => {
        //             // A return statement is branching, so it closes it current basic block
        //             // HIR statement for branching condition
        //             let stmt = hir::Statement {
        //                 kind: hir::StatementKind::Return(self.lower_expr(*expr)),
        //                 span: stmt.span,
        //             };
        //             // Add to the basic block
        //             block.push(stmt);
        //             // Finalize the block, add it to the graph
        //             let block_index = cfg.add_block(block);
        //             block_indicies.push(block_index);
        //             // We know this block will only have an edge to the exit node, so create that
        //             // here.
        //             cfg.add_edge_to_exit(block_index, ControlFlowEdge::Return);
        //             // Create a new block, preempting any other statements in this syntax block
        //             block = Block::default();
        //         }
        //         While(ref condition, ref loop_block) => {
        //             let mut partial_edges_to_next_node = HashSet::default();
        //             // Close the previous block.
        //             let block_before_loop = block;
        //             let block_before_loop_index = if block_before_loop.is_empty() {
        //                 match cfg.last_block() {
        //                     Some(block_index) => block_index,
        //                     None => cfg.entry_block,
        //                 }
        //             } else {
        //                 let block_before_loop_index = cfg.add_block(block_before_loop);
        //                 block_indicies.push(block_before_loop_index);
        //                 block_before_loop_index
        //             };
        //             // Create a new one for later...
        //             block = Block::default();
        //             // Create a new one for the loop condition
        //             let mut loop_condition_block = Block::default();
        //             // Add the condition statement
        //             loop_condition_block.push(hir::Statement {
        //                 kind: hir::StatementKind::LoopingCondition,
        //                 span: condition.span,
        //             });
        //             let loop_condition_block_index = cfg.add_block(loop_condition_block);

        //             if !cfg.block_has_early_exit(block_before_loop_index) {
        //                 cfg.add_edge(
        //                     block_before_loop_index,
        //                     loop_condition_block_index,
        //                     ControlFlowEdge::Normal,
        //                 );
        //             }

        //             block_indicies.push(loop_condition_block_index);
        //             // Lower the syntactic block for the loop

        //             let loop_body_indicies = self.lower_block_cfg(loop_block, cfg);
        //             // println!("enqueued edge {:?}", cfg.edge_queue);
        //             // The loop body has been graphed, so any queued edges will point back
        //             // to the loop condition, as that's where normal control flow will continue
        //             cfg.flush_edge_queue(loop_condition_block_index);

        //             let (loop_body_entry_node, loop_body_exit_node, loop_body_has_early_return) = branch_block_metadata!(
        //                 loop_body_indicies,
        //                 block_indicies,
        //                 cfg,
        //                 partial_edges_to_next_node
        //             );

        //             // Create an edge between the loop condition and the loop body
        //             // for when the loop condition is true.
        //             cfg.add_edge(
        //                 loop_condition_block_index,
        //                 loop_body_entry_node,
        //                 ControlFlowEdge::ConditionTrue,
        //             );
        //             // Create an edge from the loop body exit node to the loop condition,
        //             // so the condition can be checked again.
        //             if !loop_body_has_early_return {
        //                 cfg.add_edge(
        //                     loop_body_exit_node,
        //                     loop_condition_block_index,
        //                     ControlFlowEdge::Normal,
        //                 );
        //             }

        //             // Enqueue an edge to whatever block follows the loop for when the condition
        //             // is false.
        //             cfg.enqueue_edge(loop_condition_block_index, ControlFlowEdge::ConditionFalse);
        //         }
        //         _ => (),
        //     }
        // ...
        // }

        // We've reached the end of this syntactic block.

        // if !block.is_empty() {
        //     // Empty blocks are hard to deal with. In some cases we want to ignore them. For example,
        //     // if a function is empty we don't need to add this block. But in other cases, we want the
        //     // node in the graph, like when we have an if/else block that contains no statements.
        //     let block_index = cfg.add_block(block);
        //     block_indicies.push(block_index);
        // }
        // block_indicies
        // We've processed all the statements in this block.
    }

    // Computes a control flow graph for an if statement, This will be
    // treated as a sub-graph of the containing function's CFG, so it allocates
    // nodes in the outer CFG and returns the metadata needed to created the
    // required edges for the outer graph.
    // fn control_flow_graph_for_if_statement(
    //     &mut self,
    //     ast::IfExpr {
    //         block: ref if_block,
    //         ref alt,
    //         ref span,
    //         ..
    //     }: &ast::IfExpr,
    //     cfg: &mut ControlFlowGraph<hir::Statement>,
    //     mut entry_block: Block<hir::Statement>,
    // ) -> (Vec<BlockIndex>, HashSet<PartialFromEdge>) {
    //     // The set of partial edges that should be connected with the next node,
    //     // after this sub-graph is computed.
    //     let mut partial_edges_to_next_node = HashSet::default();
    //     let mut block_indicies: Vec<BlockIndex> = vec![];

    //     // Add the condition to the entry block
    //     entry_block.push(hir::Statement {
    //         kind: hir::StatementKind::BranchingCondition,
    //         span: *span,
    //     });

    //     // Add the entry block to the graph.
    //     // TODO should we just return a queue of statements instead of trying
    //     // to merge two nodes?
    //     let entry_block_index = cfg.add_block(entry_block);
    //     block_indicies.push(entry_block_index);

    //     let then_block_indicies = self.lower_block_cfg(if_block, cfg);

    //     let (then_block_entry_node, _then_block_exit_node, _then_block_has_early_return) = branch_block_metadata!(
    //         then_block_indicies,
    //         block_indicies,
    //         cfg,
    //         partial_edges_to_next_node
    //     );

    //     // Add an edge for the then block
    //     cfg.add_edge(
    //         entry_block_index,
    //         then_block_entry_node,
    //         ControlFlowEdge::ConditionTrue,
    //     );

    //     // Now we need to check for alternative branching for this if statement.
    //     // This is where things get tricky, as there could be an arbitrary number
    //     // of alternative branches if this is a series of else-if conditions.
    //     if let Some(alt) = alt {
    //         match alt {
    //             ast::Else::Block(ref block) => {
    //                 let else_block_indicies = self.lower_block_cfg(block, cfg);

    //                 let (
    //                     else_block_entry_node,
    //                     _else_block_exit_node,
    //                     _else_block_has_early_return,
    //                 ) = branch_block_metadata!(
    //                     else_block_indicies,
    //                     block_indicies,
    //                     cfg,
    //                     partial_edges_to_next_node
    //                 );
    //                 // Add a branch from the entry block to the else block when the condition
    //                 // is false
    //                 cfg.add_edge(
    //                     entry_block_index,
    //                     else_block_entry_node,
    //                     ControlFlowEdge::ConditionFalse,
    //                 );
    //             }
    //             ast::Else::If(ref else_if) => {
    //                 // Create a new block for this if else check.
    //                 let alt_entry_block = Block::default();
    //                 // Recurisvely create a sub-graph for this else-if branch. This will handled
    //                 // arbitrarily deep if-else branching.
    //                 let (alt_block_indicies, partial_edges_to_next_node_for_alt) =
    //                     self.control_flow_graph_for_if_statement(else_if, cfg, alt_entry_block);
    //                 // The partial edges for this sub-graph need to be included in the root set,
    //                 // which is what will be processed outside of this sub-graph
    //                 partial_edges_to_next_node.extend(partial_edges_to_next_node_for_alt.iter());
    //                 // Get the branch metadata for this alternative block
    //                 let (alt_block_entry_node, _alt_block_exit_node, _alt_block_has_early_return) = branch_block_metadata!(
    //                     alt_block_indicies,
    //                     block_indicies,
    //                     cfg,
    //                     partial_edges_to_next_node
    //                 );
    //                 // Add an edge from the entry block to the alt `then` block, which will execute
    //                 // if the root branch condition is false.
    //                 cfg.add_edge(
    //                     entry_block_index,
    //                     alt_block_entry_node,
    //                     ControlFlowEdge::ConditionFalse,
    //                 );
    //             }
    //         }
    //     } else {
    //         // If there is no alternative, control flow should continue on
    //         // to the next node--assuming the then block doesn't have an early return.
    //         let partial_edge = PartialFromEdge(entry_block_index, ControlFlowEdge::ConditionFalse);
    //         partial_edges_to_next_node.insert(partial_edge);
    //         // ...
    //     }
    //     (block_indicies, partial_edges_to_next_node)
    // }
}

// macro_rules! branch_block_metadata {
//     ($block_indicies: ident, $root_block_indicies: ident, $cfg: ident, $partial_edges: ident) => {
//         // It's possible that the then block is empty. If we were to compress
//         // the CFG then this node would just be ignored, but we want to
//         // potentially warn about empty blocks later so we create an empty block
//         // for now.
//         if $block_indicies.is_empty() {
//             let empty_then_block = Block::default();
//             let empty_block_index = $cfg.add_block(empty_then_block);
//             $root_block_indicies.push(empty_block_index);
//             let partial_edge = PartialFromEdge(empty_block_index, ControlFlowEdge::Normal);
//             $partial_edges.insert(partial_edge);
//             (empty_block_index, empty_block_index, false)
//         } else {
//             // Add the block indicies from the then block to the root set of
//             // block indicies
//             $root_block_indicies.extend($block_indicies.iter());
//             let block_entry_node = *$block_indicies.first().unwrap();
//             let block_exit_node = *$block_indicies.last().unwrap();
//             let block_has_early_return = $cfg.block_has_early_exit(block_exit_node);
//             // If this block exits early, add an edge to the root CFG's exit node.
//             if block_has_early_return {
//                 $cfg.add_edge_to_exit(block_exit_node, ControlFlowEdge::Return);
//             } else {
//                 // If the block doesn't exit early, queue up a partial edge, as control
//                 // will need to continue with whatever node comes after this branching
//                 // sub-graph
//                 let partial_edge = PartialFromEdge(block_exit_node, ControlFlowEdge::Normal);
//                 $partial_edges.insert(partial_edge);
//             };
//             (block_entry_node, block_exit_node, block_has_early_return)
//         }
//     };
// }