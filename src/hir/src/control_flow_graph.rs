use petgraph::dot::Dot;
use petgraph::stable_graph::{NodeIndex, StableGraph};
use petgraph::visit::Dfs;

// use petgraph::Direction;
// use petgraph::algo::{all_simple_paths, astar, dominators, has_path_connecting};

use std::collections::VecDeque;
use std::fmt;

use quickcheck::{Arbitrary, Gen, TestResult, Testable};


use rand::distributions::{Distribution};
use rand::Rng;

const MAX_MOCK_CFG_CODE_SNIPPET_DEPTH : usize = 500;

#[derive(Debug)]
struct IdentDistribution;

impl Distribution<char> for IdentDistribution {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> char {
        const RANGE: u32 = 26 + 26 + 2;
        const VALID_ASCII_IDENT_CHARS: &[u8] =
            b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                abcdefghijklmnopqrstuvwxyz\
                $_";
        loop {
            let var = rng.next_u32() >> (32 - 6);
            if var < RANGE {
                return VALID_ASCII_IDENT_CHARS[var as usize] as char
            }
        }
    }
}


/// Generate a random snippet of raw code that should
/// always give us a well-formed control-flow-graph.
#[derive(Default, Clone, Debug)]
pub struct MockControlFlowCodeSnippetBuilder {
    snippet: String,
    depth: usize,
}

impl MockControlFlowCodeSnippetBuilder {
    pub fn code(&self) -> &str {
        &self.snippet
    }

    pub fn build(&mut self) {
        self.build_block();
    }

    fn push(&mut self, code: &str) {
        // for _ in 0..self.depth {
        //     self.snippet.push(' ');
        // }
        self.snippet.push_str(code);
    }

    fn build_block(&mut self) {
        self.depth += 1;
        self.push("{\n");
        // let block_size : usize = rand::random();
        let block_size = 5;
        for _ in 0..block_size {
            self.build_stmt();
        }
        if rand::random() {
        use rand::{thread_rng, Rng};
            // Add a return statement
        let value: String = thread_rng().sample_iter(&IdentDistribution).take(5).collect();   
        self.push(&format!("return \"{}\";", value));
        }
        self.push("\n}");
        self.depth -= 1;
    }

    fn build_stmt(&mut self) {
        if self.snippet.len() >= MAX_MOCK_CFG_CODE_SNIPPET_DEPTH {
            return;
        }
        // Eeach statement has equal random chance
        if rand::random() {
            self.build_non_branching_stmt();
        } else if rand::random() {
            self.build_if_branch();
        } else if rand::random() {
            self.build_while_loop();
        }
    }

    fn build_non_branching_stmt(&mut self) {
        use rand::distributions::Alphanumeric;
        use rand::{thread_rng, Rng};
        let name: String = thread_rng().sample_iter(&IdentDistribution).take(5).collect();
        let value: String = thread_rng().sample_iter(&IdentDistribution).take(5).collect();
        let code = format!("let {} = \"{}\";\n", name, value);
        self.push(&code);
    }

    fn build_if_branch(&mut self) {
        self.push("if true ");
        self.build_block();
        // Else-If chain
        if rand::random() {
            // self.push(" else ");
        }
        // Else block
        else if rand::random() {
            self.push(" else ");
            self.build_block();
        }
        self.push(";");
    }

    fn build_while_loop(&mut self) {
        self.push("while true ");
        self.build_block();
        self.push(";");
    }
}

impl Arbitrary for MockControlFlowCodeSnippetBuilder {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        let mut builder = MockControlFlowCodeSnippetBuilder::default();
        builder.build();
        builder
    }
}

impl Arbitrary for ControlFlowGraph {
    fn arbitrary<G: Gen>(g: &mut G) -> Self {
        /// Start with an empty graph
        let mut cfg = ControlFlowGraph::default();
        // ...
        cfg
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BlockIndex(NodeIndex);

use crate::hir;

pub struct SubControlFlowGraph<'cfg> {
    cfg: &'cfg mut ControlFlowGraph,
}

/// A control flow graph for a single function
#[derive(Debug, Clone)]
pub struct ControlFlowGraph {
    /// The graph data structure
    graph: StableGraph<BasicBlock, ControlFlowEdge>,
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

impl Default for ControlFlowGraph {
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

impl ControlFlowGraph {
    /// Add a new basic block to the grapg
    pub fn add_block(&mut self, block: Block) -> BlockIndex {
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

    pub fn dfs(&self) {
        let mut dfs = Dfs::new(&self.graph, self.entry_block.0);
        while let Some(index) = dfs.next(&self.graph) {
            println!("node {:?}", index);
        }
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
                    use crate::hir::StatementKind::*;
                    match stmt.kind {
                        Return => true,
                        _ => false,
                    }
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

    // ...
}

#[derive(Default, Clone)]
pub struct Block {
    // The set of non-branching statements within the block.
    pub statements: Vec<hir::Statement>,
}

// Only print the number of statements in a block, makes debugging easier for now
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.statements)
    }
}

impl Block {
    /// Push a statement into the basic block
    pub fn push(&mut self, stmt: hir::Statement) {
        self.statements.push(stmt);
    }

    /// returns whether this block contains any statements
    pub fn is_empty(&self) -> bool {
        self.statements.is_empty()
    }

    pub fn exit_type(&self) -> BlockExit {
        match self.statements.last() {
            Some(stmt) => {
                use crate::hir::StatementKind;
                match stmt.kind {
                    StatementKind::BranchingCondition => BlockExit::BranchCondition,
                    StatementKind::LoopingCondition => BlockExit::LoopCondition,
                    StatementKind::Return => BlockExit::Return,
                    _ => BlockExit::Normal,
                }
            }
            None => BlockExit::Normal,
        }
    }
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
pub enum BasicBlock {
    /// A special block denoting the entry-point of the graph.
    Entry,
    /// A basic block.
    Block(Block),
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
