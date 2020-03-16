use crate::arena::{Arena, Id};
use crate::HashMap;

use std::fmt::Debug;
use std::hash::Hash;

pub trait Reference: Debug + Eq + Hash {}

pub trait Referant: Debug + Clone {}

/// An individual scope. Mapped to a block or module, as those are the
/// only language items that allow for scope creation.
#[derive(Debug)]
pub struct Scope<K: Reference, V: Referant> {
    bindings: HashMap<K, V>,
}

impl<K: Reference, V: Referant> Default for Scope<K, V> {
    fn default() -> Self {
        Scope {
            bindings: HashMap::default(),
        }
    }
}

impl<K: Reference, V: Referant> Scope<K, V> {
    pub fn define(&mut self, reference: K, referant: V) {
        self.bindings.insert(reference, referant);
    }
    pub fn resolve(&mut self, reference: &K) -> Option<V> {
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
    // A stack of definitions that are referenceable, but
    // that haven't actually been defined. This lets us
    // handle recurisve function calls.
    // TODO use this
    pending_defs: Vec<K>,
}

impl<K: Reference, V: Referant> Default for ScopeMap<K, V> {
    fn default() -> Self {
        ScopeMap {
            scope_arena: Arena::new(),
            active_scopes: vec![],
            pending_defs: vec![],
        }
    }
}

impl<K: Reference, V: Referant> ScopeMap<K, V> {
    /// Define a new binding
    pub fn define(&mut self, reference: K, referant: V) {
        match self.scope_arena.get_mut(self.current_scope_id()) {
            Some(scope) => scope.define(reference, referant),
            None => panic!(),
        };
        // ...
    }

    pub fn enter_scope(&mut self) {
        // Create a new scope
        let scope = Scope::default();
        // Put the scope into the persisted arena.
        let scope_id = self.scope_arena.alloc(scope);
        // Push the scope ID into the list of active scopes
        self.active_scopes.push(scope_id)
    }

    pub fn exit_scope(&mut self) {
        // Remove the last scope from the list of referencable scopes
        self.active_scopes.pop();
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
    pub fn resolve(&mut self, reference: &K) -> Option<V> {
        // Walk through the active scopes backwards, as we want to attempt
        // resolution with the *newest* scope first.
        for scope_id in self.active_scopes.iter().rev() {
            let scope = self.scope_arena.get_mut(*scope_id);
            match scope {
                Some(scope) => match scope.resolve(&reference) {
                    Some(binding) => {
                        // scope.
                        // TODO register that this binding has been referenced, so we can later
                        // remove bindings that are never referenced.
                        return Some(binding);
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
