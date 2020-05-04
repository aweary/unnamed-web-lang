use petgraph::graph::{EdgeIndex, Graph, NodeIndex};

use crate::arena::{Arena, Id};
use crate::scope_map::Reference;

use std::clone::Clone;
use std::cmp::Eq;
use std::fmt::Debug;
use std::hash::{Hash, Hasher};

// A module in the module graph
#[derive(Debug)]
pub struct ModuleGraphIndex<M>(Id<M>, NodeIndex);

impl<T> Hash for ModuleGraphIndex<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
        self.1.hash(state);
    }
}

impl<T> PartialEq for ModuleGraphIndex<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.0 == rhs.0 && self.1 == rhs.1
    }
}

impl<T> Eq for ModuleGraphIndex<T> {}

impl<M> Clone for ModuleGraphIndex<M> {
    fn clone(&self) -> Self {
        ModuleGraphIndex(self.0, self.1)
    }
}

impl<M> Copy for ModuleGraphIndex<M> {}

impl<M: Debug> ModuleGraphIndex<M> {
    /// Resolve a module from the provided ModuleGraph for the index
    pub fn resolve<'a, D: Reference>(
        &self,
        graph: &'a ModuleGraph<M, D>,
    ) -> &'a M {
        graph
            .modules
            .get(self.0)
            .expect("ModuleGraphIndex must always point to a valid module")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ModuleDependency<T: Reference>(T, EdgeIndex);

pub struct ModuleGraph<M: Debug, D: Reference> {
    pub modules: Arena<M>,
    pub graph: Graph<Id<M>, D>,
}

impl<M: Debug, D: Reference> ModuleGraph<M, D> {
    pub fn add(&mut self, module: M) -> ModuleGraphIndex<M> {
        let id = self.modules.alloc(module);
        let index = self.graph.add_node(id);
        ModuleGraphIndex(id, index)
    }

    pub fn add_dep(
        &mut self,
        a: ModuleGraphIndex<M>,
        b: ModuleGraphIndex<M>,
        import_name: D,
    ) -> ModuleDependency<D> {
        let (a_index, b_index) = (a.1, b.1);
        let edge_index =
            self.graph.add_edge(a_index, b_index, import_name.clone());
        ModuleDependency(import_name, edge_index)
    }

    pub fn get_mut(&mut self, index: ModuleGraphIndex<M>) -> &mut M {
        self.modules.get_mut(index.0).unwrap()
    }

    pub fn get(&self, index: ModuleGraphIndex<M>) -> &M {
        self.modules.get(index.0).unwrap()
    }

    pub fn with<F>(&self, index: ModuleGraphIndex<M>, func: F)
    where
        F: FnOnce(&M) -> (),
    {
        let module = self.get(index);
        func(module);
    }
}

impl<M: Debug, D: Reference> std::default::Default for ModuleGraph<M, D> {
    fn default() -> Self {
        ModuleGraph {
            graph: Graph::default(),
            modules: Arena::default(),
        }
    }
}

impl<M: Debug, D: Reference> std::fmt::Debug for ModuleGraph<M, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Custom implementation since Graph doesn't support Debug
        write!(f, "ModuleGraph(...)")
    }
}
