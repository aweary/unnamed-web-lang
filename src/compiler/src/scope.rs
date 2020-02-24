use crate::ir::*;
use fxhash::FxHashMap;

use id_arena::{Arena, Id};
use syntax::symbol::Symbol;

type ScopeId = Id<Scope>;

// Just an alias for semantic naming
pub type Reference = Ident;

// Just an alias for semantic naming
pub type BindingName = Symbol;

/// An individual scope. Mapped to a block or module, as those are the
/// only language items that allow for scope creation.
#[derive(Default, Debug)]
pub struct Scope {
    bindings: FxHashMap<BindingName, Binding>,
}

impl Scope {
    pub fn define(&mut self, reference: Reference, binding: Binding) {
        self.bindings.insert(reference.name, binding);
    }
    pub fn resolve(&mut self, reference: &Reference) -> Option<Binding> {
        self.bindings.get(&reference.name).cloned()
    }
}

/// ScopeTracker is responsible for tracking and persisting scope information,
/// including reference counting.
#[derive(Default, Debug)]
pub struct ScopeTracker {
    scope_arena: Arena<Scope>,
    active_scopes: Vec<ScopeId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Binding {
    /// A binding for a function definition.
    Func(FuncDefId),
    /// A binding for a local
    Local(LocalId),
    /// A binding for top-level item, which we don't know
    /// the type of.
    Item(ItemId),
    // TODO what does this point to?
    Param,
}

impl ScopeTracker {
    /// Define a new binding
    pub fn define(&mut self, reference: Reference, binding: Binding) {
        match self.scope_arena.get_mut(self.current_scope_id()) {
            Some(scope) => scope.define(reference, binding),
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

    pub fn current_scope_id(&self) -> ScopeId {
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
    pub fn resolve(&mut self, reference: &Reference) -> Option<Binding> {
        // Walk through the active scopes backwards, as we want to attempt
        // resolution with the *newest* scope first.
        for scope_id in self.active_scopes.iter().rev() {
            let scope = self.scope_arena.get_mut(*scope_id);
            match scope {
                Some(scope) => match scope.resolve(&reference) {
                    Some(binding) => {
                        // scope.
                        println!("Resolved {:?} from reference {:?}", binding, reference);
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
