use petgraph::dot::Dot;
use petgraph::stable_graph::{NodeIndex, StableGraph};

use std::collections::VecDeque;
use std::fmt::{self, Debug};

#[derive(Debug)]
struct IdentDistribution;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockIndex(NodeIndex);


pub trait Blockable : Debug {
    fn has_early_exit(&self) -> bool;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PartialFromEdge(pub BlockIndex, pub ControlFlowEdge);

// struct PartialToEdge(BlockIndex, ControlFlowEdge);

/// A control flow graph for a single function
#[derive(Debug, Clone)]
pub struct ControlFlowGraph<T: Blockable> {
    /// The graph data structure
    graph: StableGraph<BasicBlock<T>, ControlFlowEdge>,
    /// The entry block of the graph
    /// TODO make private
    pub entry_block: BlockIndex,
    /// The exit block of the graph
    exit_block: BlockIndex,
    /// The first basic block block
    first_block: Option<BlockIndex>,
    /// The last block added to the graph
    last_block: Option<BlockIndex>,
    /// A queue of edges that will be flushed when a block is added
    edge_queue: VecDeque<(BlockIndex, ControlFlowEdge)>,
}

impl<T: Blockable> Default for ControlFlowGraph<T> {
    fn default() -> Self {
        let mut graph = StableGraph::default();
        // Initialize the special entry and exit blocks
        let entry_block = BlockIndex(graph.add_node(BasicBlock::Entry));
        let exit_block = BlockIndex(graph.add_node(BasicBlock::Exit));
        // Initialize the first basic block, since we know that
        // we will at the very least have one
        ControlFlowGraph {
            graph,
            entry_block,
            exit_block,
            first_block: None,
            last_block: None,
            edge_queue: VecDeque::default(),
        }
    }
}

impl<T : Blockable> ControlFlowGraph<T> {
    /// Add a new basic block to the grapg
    pub fn add_block(&mut self, block: Block<T>) -> BlockIndex {
        let block_index = BlockIndex(self.graph.add_node(BasicBlock::Block(block)));
        // If this is the first block to be added to the graph, add an edge from the
        // special entry block to this one.
        if self.first_block.is_none() {
            self.first_block = Some(block_index);
            self.add_edge(self.entry_block, block_index, ControlFlowEdge::Normal);
        }

        // Flush the edge
        for (from_block, edge) in self.edge_queue.clone() {
            self.add_edge(from_block, block_index, edge);
        }
        // Clear out the edge queue
        self.edge_queue.clear();
        self.last_block = Some(block_index);
        block_index
    }

    /// Get the last basic block added to the graph, or `None` if it's empty
    pub fn last_block(&self) -> Option<BlockIndex> {
        self.last_block
    }

    /// Add an edge from a block to the special exit block
    pub fn add_edge_to_exit(&mut self, index: BlockIndex, edge: ControlFlowEdge) {
        self.add_edge(index, self.exit_block, edge);
    }

    /// Add an edge between two blocks
    pub fn add_edge(&mut self, from: BlockIndex, to: BlockIndex, edge: ControlFlowEdge) {
        self.graph.update_edge(from.0, to.0, edge);
    }

    /// Queues up an edge for the next block added to the graph, where `block_index` is the
    /// source block.
    pub fn enqueue_edge(&mut self, block_index: BlockIndex, edge: ControlFlowEdge) {
        self.edge_queue.push_back((block_index, edge));
    }

    /// Returns whether a given block has early exit semantics. In other words, it tells
    /// us whether we need to account for the default top-down flow for non-early exiting
    /// statements.
    pub fn block_has_early_exit(&self, block_index: BlockIndex) -> bool {
        match &self.graph[block_index.0] {
            BasicBlock::Block(block) => {
                if let Some(stmt) = block.statements.last() {
                    stmt.has_early_exit()
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Flush the edge queue using an arbitrary block as the target
    pub fn flush_edge_queue(&mut self, block_index: BlockIndex) {
        // Flush the edge queue
        for (from_block, edge) in self.edge_queue.clone() {
            self.graph.update_edge(from_block.0, block_index.0, edge);
        }
        // Clear out the edge queue
        self.edge_queue.clear();
    }

    /// Takes all the queued edges and connects them with the special exit
    /// block. This signals that CFG construction is completed.
    /// We also check if the graph is empty, and connect the
    /// entry and exit blocks if so.
    pub fn flush_edge_queue_to_exit_block(&mut self) {
        self.flush_edge_queue(self.exit_block);
        // If there is no last block, nothing was ever added to this
        // graph.
        if self.last_block.is_none() {
            self.add_edge(self.entry_block, self.exit_block, ControlFlowEdge::Normal);
        }
    }

    pub fn print(&self) {
        println!("{:?}", Dot::with_config(&self.graph, &[]));
    }

    pub fn graphviz_output(&self) -> String {
        format!("{:?}", Dot::with_config(&self.graph, &[]))
    }
}

#[derive(Clone)]
pub struct Block<T: Blockable> {
    // The set of non-branching statements within the block.
    pub statements: Vec<T>,
}

impl<T: Blockable> Default for Block<T> {
    fn default() -> Self {
        Block { statements: vec![] }
    }
}

// Only print the number of statements in a block, makes debugging easier for now
impl<T: Blockable> fmt::Debug for Block<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.statements)
    }
}

impl<T : Blockable> Block<T> {
    /// Push a statement into the basic block
    pub fn push(&mut self, stmt: T) {
        self.statements.push(stmt);
    }

    /// returns whether this block contains any statements
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    // pub fn exit_type(&self) -> BlockExit {
    //     match self.statements.last() {
    //         Some(stmt) => {
    //             use crate::hir::StatementKind;
    //             match stmt.kind {
    //                 StatementKind::BranchingCondition => BlockExit::BranchCondition,
    //                 StatementKind::LoopingCondition => BlockExit::LoopCondition,
    //                 StatementKind::Return => BlockExit::Return,
    //                 _ => BlockExit::Normal,
    //             }
    //         }
    //         None => BlockExit::Normal,
    //     }
    // }
}

#[derive(Debug, Clone)]
pub enum BlockExit {
    // Placeholder, we don't know how the block exits yet
    Unknown,
    // The block forks based on a boolean condition. It will
    // have edges to other blocks for both `true` and `false`
    BranchCondition,
    LoopCondition,
    // The block ends with a return statement. It will have a
    // single edge to the special exit block.
    Return,
    // The block naturally flows to another block, e.g., when
    // an `if` block finishes executing.
    Normal,
}

#[derive(Debug, Clone)]
pub enum BasicBlock<T : Blockable> {
    /// A special block denoting the entry-point of the graph.
    Entry,
    /// A basic block.
    Block(Block<T>),
    // Special block denoting the exit-point of the graph.
    Exit,
}

/// An edge between basic blocks. It answers the question
/// "how can control flow move between blocks?"
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlFlowEdge {
    /// Normal control flow is what occurs as code is executed
    /// top-down. For example, when an `if` without an `else` clause
    /// ends it naturally moves control flow back outside the block.
    Normal,
    /// When control flow depends on some boolean condition, this is
    /// the page taken when the condition evalutes to `true`.
    ConditionTrue,
    /// Same as ConditionTrue, but for `false`
    ConditionFalse,
    /// An explicit return statement, always goes straight to the exit block.
    Return,
}
