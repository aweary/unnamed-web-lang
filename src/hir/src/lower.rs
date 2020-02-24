use crate::control_flow_graph::{Block, BlockExit, BlockIndex, ControlFlowEdge, ControlFlowGraph};
use crate::hir;

use fxhash::FxHashSet;
use syntax::ast;



macro_rules! branch_block_metadata {
    ($block_indicies: ident, $root_block_indicies: ident, $cfg: ident, $partial_edges: ident) => {
        // It's possible that the then block is empty. If we were to compress
        // the CFG then this node would just be ignored, but we want to
        // potentially warn about empty blocks later so we create an empty block
        // for now.
        if $block_indicies.is_empty() {
            let empty_then_block = Block::default();
            let empty_block_index = $cfg.add_block(empty_then_block);
            $root_block_indicies.push(empty_block_index);
            let partial_edge = PartialFromEdge(empty_block_index, ControlFlowEdge::Normal);
            $partial_edges.insert(partial_edge);
            (empty_block_index, empty_block_index, false)
        } else {
            // Add the block indicies from the then block to the root set of
            // block indicies
            $root_block_indicies.extend($block_indicies.iter());
            let block_entry_node = *$block_indicies.first().unwrap();
            let block_exit_node = *$block_indicies.last().unwrap();
            let block_has_early_return = $cfg.block_has_early_exit(block_exit_node);
            // If this block exits early, add an edge to the root CFG's exit node.
            if block_has_early_return {
                $cfg.add_edge_to_exit(block_exit_node, ControlFlowEdge::Return);
            } else {
                // If the block doesn't exit early, queue up a partial edge, as control
                // will need to continue with whatever node comes after this branching
                // sub-graph
                let partial_edge = PartialFromEdge(block_exit_node, ControlFlowEdge::Normal);
                $partial_edges.insert(partial_edge);
            };
            (block_entry_node, block_exit_node, block_has_early_return)
        }
    };
}

pub fn lower_module(module: ast::Mod) {
    for item in module.items {
        if let ast::ItemKind::Fn(fn_def) = item.kind {
            lower_fn(fn_def);
            // ...
        };
    }
    // println!("Lowering module!");
}

pub(crate) fn lower_fn(fn_def: ast::FnDef) -> hir::Function {
    // The control flow graph for this function
    let mut cfg = ControlFlowGraph::default();
    // Initial function definition
    let fn_body_graph = lower_block(&*fn_def.body, &mut cfg);
    // Add an edge from the last item to the exit node
    if let Some(fn_body_graph_exit_node) = fn_body_graph.last() {
        if !cfg.block_has_early_exit(*fn_body_graph_exit_node) {
            cfg.add_edge_to_exit(*fn_body_graph_exit_node, ControlFlowEdge::Normal);
        }
    }
    // Any queued edges will point to the exit node, as we're done with the graph
    cfg.flush_edge_queue_to_exit_block();
    // cfg.print();
    hir::Function {
        span: fn_def.span,
        name: fn_def.name,
        graph: cfg,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PartialFromEdge(BlockIndex, ControlFlowEdge);
struct PartialToEdge(BlockIndex, ControlFlowEdge);

/// Computes a control flow graph for an if statement, This will be
/// treated as a sub-graph of the containing function's CFG, so it allocates
/// nodes in the outer CFG and returns the metadata needed to created the
/// required edges for the outer graph.
fn control_flow_graph_for_if_statement(
    ast::IfExpr {
        block: ref if_block,
        ref alt,
        ref span,
        ..
    }: &ast::IfExpr,
    cfg: &mut ControlFlowGraph,
    mut entry_block: Block,
) -> (Vec<BlockIndex>, FxHashSet<PartialFromEdge>) {
    // The set of partial edges that should be connected with the next node,
    // after this sub-graph is computed.
    let mut partial_edges_to_next_node = FxHashSet::default();
    let mut block_indicies: Vec<BlockIndex> = vec![];

    // Add the condition to the entry block
    entry_block.push(hir::Statement {
        kind: hir::StatementKind::BranchingCondition,
        span: *span,
    });

    // Add the entry block to the graph.
    // TODO should we just return a queue of statements instead of trying
    // to merge two nodes?
    let entry_block_index = cfg.add_block(entry_block);
    block_indicies.push(entry_block_index);

    let then_block_indicies = lower_block(if_block, cfg);

    let (then_block_entry_node, then_block_exit_node, then_block_has_early_return) = branch_block_metadata!(
        then_block_indicies,
        block_indicies,
        cfg,
        partial_edges_to_next_node
    );

    // Add an edge for the then block
    cfg.add_edge(
        entry_block_index,
        then_block_entry_node,
        ControlFlowEdge::ConditionTrue,
    );

    // Now we need to check for alternative branching for this if statement.
    // This is where things get tricky, as there could be an arbitrary number
    // of alternative branches if this is a series of else-if conditions.
    if let Some(alt) = alt {
        match alt {
            ast::Else::Block(ref block) => {
                let else_block_indicies = lower_block(block, cfg);

                // TODO dedupe with above
                let (else_block_entry_node, else_block_exit_node, else_block_has_early_return) = branch_block_metadata!(
                    else_block_indicies,
                    block_indicies,
                    cfg,
                    partial_edges_to_next_node
                );
                // Add a branch from the entry block to the else block when the condition
                // is false
                cfg.add_edge(
                    entry_block_index,
                    else_block_entry_node,
                    ControlFlowEdge::ConditionFalse,
                );
            }
            ast::Else::If(ref else_if) => {
                // Create a new block for this if else check.
                let alt_entry_block = Block::default();
                // Recurisvely create a sub-graph for this else-if branch. This will handled
                // arbitrarily deep if-else branching.
                let (alt_block_indicies, partial_edges_to_next_node_for_alt) =
                    control_flow_graph_for_if_statement(else_if, cfg, alt_entry_block);
                // The partial edges for this sub-graph need to be included in the root set,
                // which is what will be processed outside of this sub-graph
                partial_edges_to_next_node.extend(partial_edges_to_next_node_for_alt.iter());
                // Get the branch metadata for this alternative block
                let (alt_block_entry_node, alt_block_exit_node, alt_block_has_early_return) = branch_block_metadata!(
                    alt_block_indicies,
                    block_indicies,
                    cfg,
                    partial_edges_to_next_node
                );
                // Add an edge from the entry block to the alt `then` block, which will execute
                // if the root branch condition is false.
                cfg.add_edge(
                    entry_block_index,
                    alt_block_entry_node,
                    ControlFlowEdge::ConditionFalse,
                );
            }
        }
    } else {
        // If there is no alternative, control flow should continue on
        // to the next node--assuming the then block doesn't have an early return.
        let partial_edge = PartialFromEdge(entry_block_index, ControlFlowEdge::ConditionFalse);
        partial_edges_to_next_node.insert(partial_edge);
        // ...
    }
    (block_indicies, partial_edges_to_next_node)
}

fn lower_block(body: &ast::Block, cfg: &mut ControlFlowGraph) -> Vec<BlockIndex> {
    // Start off by creating a basic block. It's possible this block might be empty,
    // if this syntactic block is empty.
    let mut block_indicies = vec![];
    let mut block = Block::default();
    for stmt in &body.stmts {
        use ast::StmtKind::*;
        match stmt.kind {
            Local(_) => {
                // HIR statement node
                let stmt = hir::Statement {
                    kind: hir::StatementKind::LocalDefinition,
                    span: stmt.span,
                };
                // Add it to the current basic block
                block.push(stmt);
                // ...
            }
            If(ref if_expr) => {
                let branching_block = block;
                block = Block::default();
                let (if_branch_indicies, partial_edges) =
                    control_flow_graph_for_if_statement(if_expr, cfg, branching_block);
                block_indicies.extend(if_branch_indicies.iter());
                for PartialFromEdge(block_index, edge) in partial_edges {
                    cfg.enqueue_edge(block_index, edge);
                    // ...
                }
            }
            Return(_) => {
                // A return statement is branching, so it closes it current basic block
                // HIR statement for branching condition
                let stmt = hir::Statement {
                    kind: hir::StatementKind::Return,
                    span: stmt.span,
                };
                // Add to the basic block
                block.push(stmt);
                // Finalize the block, add it to the graph
                let block_index = cfg.add_block(block);
                block_indicies.push(block_index);
                // We know this block will only have an edge to the exit node, so create that
                // here.
                cfg.add_edge_to_exit(block_index, ControlFlowEdge::Return);
                // Create a new block, preempting any other statements in this syntax block
                block = Block::default();
            }
            While(ref condition, ref loop_block) => {
                let mut partial_edges_to_next_node = FxHashSet::default();
                // Close the previous block.
                let block_before_loop = block;
                let block_before_loop_index = if block_before_loop.is_empty() {
                    match cfg.last_block() {
                        Some(block_index) => block_index,
                        None => cfg.entry_block
                    }
                } else {
                    let block_before_loop_index = cfg.add_block(block_before_loop);
                    block_indicies.push(block_before_loop_index);
                    block_before_loop_index
                };
                // Create a new one for later...
                block = Block::default();
                // Create a new one for the loop condition
                let mut loop_condition_block = Block::default();
                // Add the condition statement
                loop_condition_block.push(hir::Statement {
                    kind: hir::StatementKind::LoopingCondition,
                    span: condition.span,
                });
                let loop_condition_block_index = cfg.add_block(loop_condition_block);

                if !cfg.block_has_early_exit(block_before_loop_index) {
                    cfg.add_edge(
                        block_before_loop_index,
                        loop_condition_block_index,
                        ControlFlowEdge::Normal,
                    );
                }

                block_indicies.push(loop_condition_block_index);
                // Lower the syntactic block for the loop

                let loop_body_indicies = lower_block(loop_block, cfg);
                // println!("enqueued edge {:?}", cfg.edge_queue);
                // The loop body has been graphed, so any queued edges will point back
                // to the loop condition, as that's where normal control flow will continue
                cfg.flush_edge_queue(loop_condition_block_index);

                let (loop_body_entry_node, loop_body_exit_node, loop_body_has_early_return) = branch_block_metadata!(
                    loop_body_indicies,
                    block_indicies,
                    cfg,
                    partial_edges_to_next_node
                );

                println!("loop_block_entry_node {:?}", loop_body_indicies);

                // Create an edge between the loop condition and the loop body
                // for when the loop condition is true.
                cfg.add_edge(
                    loop_condition_block_index,
                    loop_body_entry_node,
                    ControlFlowEdge::ConditionTrue,
                );
                // Create an edge from the loop body exit node to the loop condition,
                // so the condition can be checked again.
                if !loop_body_has_early_return {
                    cfg.add_edge(
                        loop_body_exit_node,
                        loop_condition_block_index,
                        ControlFlowEdge::Normal,
                    );
                }

                // Enqueue an edge to whatever block follows the loop for when the condition
                // is false.
                cfg.enqueue_edge(loop_condition_block_index, ControlFlowEdge::ConditionFalse);
            }
            _ => (),
        }
        // ...
    }

    // We've reached the end of this syntactic block.

    if !block.is_empty() {
        // Empty blocks are hard to deal with. In some cases we want to ignore them. For example,
        // if a function is empty we don't need to add this block. But in other cases, we want the
        // node in the graph, like when we have an if/else block that contains no statements.
        let block_index = cfg.add_block(block);
        block_indicies.push(block_index);
    }
    block_indicies
    // We've processed all the statements in this block.
}
