use petgraph::graph::Graph;

use crate::ir::{ExprId, DefId};

/**
 * A basic block is a set of statements with no branching
 * behavior. They are the nodes in the CFG
 */
pub struct BasicBlock {
    stmts: Vec<DefId>,
}

/**
 *
 */
pub struct CFGEdge {
    // ID for the expression in the condition
    condition: ExprId,
}

/**
 * A CFG used in Components, Functions, and Blocks. Enables us to generate
 * a union of potential values for this to typecheck against
 */
pub struct CFG {
    graph: Graph<BasicBlock, CFGEdge>,
}

impl CFG {
    pub fn new() -> Self {
        let graph = Graph::new();
        Self { graph }
    }
}
