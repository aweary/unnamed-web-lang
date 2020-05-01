use crate::arena::{Arena, Id};
use crate::HashMap;

use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;

pub trait Reference: Debug + Eq + Hash + Clone {}

pub trait Referant: Debug + Clone {}

/// A unique reference is a scope-aware, unique identifier
/// for a referencable value. When a value is defined in the scope
/// map we generate and return a unique reference so that the HIR
/// can use this instead of the potentially-non-unique identifier
/// from the source code. The `ScopeMap` manages genearting the unique
/// u16 values.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct UniqueReference<K: Reference>(u16, PhantomData<K>);

impl<T: Reference> Copy for UniqueReference<T> {}

/// An individual scope. Mapped to a block or module, as those are the
/// only language items that allow for scope creation.
#[derive(Debug)]
pub struct Scope<K: Reference, V: Referant> {
    bindings: HashMap<K, (V, UniqueReference<K>)>,
}

impl<K: Reference, V: Referant> Default for Scope<K, V> {
    fn default() -> Self {
        Scope {
            bindings: HashMap::default(),
        }
    }
}

impl<K: Reference, V: Referant> Scope<K, V> {
    pub fn define(&mut self, reference: K, referant: V, unique_reference: UniqueReference<K>) {
        self.bindings.insert(reference, (referant, unique_reference));
    }
    pub fn resolve(&mut self, reference: &K) -> Option<(V, UniqueReference<K>)> {
        self.bindings.get(&reference).cloned()
    }
}

type ScopeId<K, V> = Id<Scope<K, V>>;

/// ScopeMap is responsible for tracking and persisting scope information,
/// including reference counting.
#[derive(Debug)]
pub struct ScopeMap<K: Reference, V: Referant> {
    // The memory arena we allocate scopes from
    scope_arena: Arena<Scope<K, V>>,
    // The list of scopes that are currently active
    active_scopes: Vec<ScopeId<K, V>>,
    // Map of scope IDs to the list of other scopes that they have access to
    scope_chain_map: HashMap<ScopeId<K, V>, Vec<ScopeId<K, V>>>,
    // Map of unique references to their non-unique counterparts
    unique_map: HashMap<UniqueReference<K>, K>,
    // A stack of definitions that are referenceable, but
    // that haven't actually been defined. This lets us
    // handle recurisve function calls.
    // TODO use this
    pending_defs: Vec<K>,
    // Used for generating unique reference names/ids
    unique_id: u16,
}

impl<K: Reference, V: Referant> Default for ScopeMap<K, V> {
    fn default() -> Self {
        ScopeMap {
            scope_arena: Arena::new(),
            active_scopes: vec![],
            scope_chain_map: HashMap::default(),
            unique_map: HashMap::default(),
            pending_defs: vec![],
            unique_id: 0,
        }
    }
}

impl<K: Reference + Debug, V: Referant> ScopeMap<K, V> {
    fn generate_unique_reference(&mut self) -> UniqueReference<K> {
        let id = self.unique_id;
        self.unique_id += 1;
        UniqueReference(id, PhantomData)
    }

    /// Define a new binding, returning a unique identifier for the binding,
    /// as well as a ScopeId which can be used to read all the values in that
    /// scope later
    pub fn define(&mut self, reference: K, referant: V) -> (UniqueReference<K>, ScopeId<K, V>) {
        let scope_id = self.current_scope_id();
        // Create a unique reference for this definition
        let unique_reference = self.generate_unique_reference();
        match self.scope_arena.get_mut(scope_id) {
            Some(scope) => {
                // self.unique_map.insert(unique_reference.clone(), reference);
                scope.define(reference, referant, unique_reference);
                (unique_reference, scope_id)
            }
            None => panic!(),
        }
    }

    pub fn enter_scope(&mut self) {
        let scope = Scope::default();
        let scope_id = self.scope_arena.alloc(scope);
        self.active_scopes.push(scope_id)
    }

    pub fn exit_scope(&mut self) -> ScopeId<K, V> {
        let scope = self
            .active_scopes
            .pop()
            .expect("there must always be a scope");
        let chain = self.active_scopes.clone();
        self.scope_chain_map.insert(scope, chain);
        scope
    }

    pub fn current_scope_id(&self) -> ScopeId<K, V> {
        *self
            .active_scopes
            .last()
            .expect("there must always be a scope")
    }

    // pub fn find_similar(&self, target: &Symbol) -> Option<(&Symbol, &Binding)> {
    //     let mut result = None;
    //     let mut lowest_distance = std::usize::MAX;
    //     for scope in self.scope_maps.iter().rev() {
    //         for name in scope.keys() {
    //             let distance = edit_distance(target.as_str(), name.as_str());
    //             if distance < lowest_distance {
    //                 lowest_distance = distance;
    //                 result = Some((name, scope.get(name).unwrap()))
    //             }
    //             // ...
    //         }
    //     }
    //     // Only make a recommendation if the edit distance is low
    //     if lowest_distance < 3 {
    //         result
    //     } else {
    //         None
    //     }
    // }

    /// Resolve a reference to a binding, if it exists. If a binding does exist,
    /// this will track that it is being referenced to by `reference`.
    pub fn resolve(&mut self, reference: &K) -> Option<(V, UniqueReference<K>)> {
        // Walk through the active scopes backwards, as we want to attempt
        // resolution with the *newest* scope first.
        for scope_id in self.active_scopes.iter().rev() {
            let scope = self.scope_arena.get_mut(*scope_id);
            match scope {
                Some(scope) => match scope.resolve(&reference) {
                    Some((binding, unique_reference)) => {
                        // scope.
                        // TODO register that this binding has been referenced, so we can later
                        // remove bindings that are never referenced.
                        return Some((binding, unique_reference));
                    }
                    None => {
                        continue;
                    }
                },
                None => panic!(),
            }
        }
        None
    }
}
