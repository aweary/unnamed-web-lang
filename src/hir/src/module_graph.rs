// use fxhash::FxHashMap;
// use petgraph::graph::{EdgeIndex, NodeIndex};
// use petgraph::Graph;

// // TODO import from IR
// use crate::arena::ModuleId;

// #[derive(Debug, Clone, Copy)]
// pub struct ModuleGraphNode(pub(crate) NodeIndex);
// #[derive(Debug, Clone, Copy)]
// pub struct ModuleGraphDependency(pub(crate) EdgeIndex);

// // An import is the only way for modules
// // to be connected in the module graph.
// pub struct Import {
//     // ...
// }

// pub struct ModuleGraph {
//     graph: Graph<ModuleId, Import>,
//     module_to_index_map: FxHashMap<ModuleId, ModuleGraphNode>,
// }

// impl std::fmt::Debug for ModuleGraph {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         // Custom implementation since Graph doesn't support Debug
//         write!(f, "ModuleGraph(...)")
//     }
// }

// impl ModuleGraph {
//     /// Add a new module to the graph
//     pub fn add(&mut self, module_id: ModuleId) -> ModuleGraphNode {
//         let index = self.graph.add_node(module_id);
//         let node = ModuleGraphNode(index);
//         self.module_to_index_map.insert(module_id, node);
//         node
//     }

//     /// Add a dependency between two modules
//     pub fn add_dep(&mut self, a: ModuleId, b: ModuleId, dep: Import) {
//         if let (Some(a_node), Some(b_node)) = (
//             self.module_to_index_map.get(&a),
//             self.module_to_index_map.get(&b),
//         ) {
//             self.graph.add_edge(a_node.0, b_node.0, dep);
//         } else {
//             unreachable!();
//         }
//     }
// }
