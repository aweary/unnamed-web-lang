use diagnostics::ParseResult as Result;
use hir::unique_name::UniqueName;
use std::collections::{BTreeSet, HashMap, VecDeque};
use syntax::symbol::Symbol;
use ty::{Existential, Type};

use internment::Intern;
use log::debug;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementKind {
    /// An existential type
    Existential(Existential),
    /// An unsolved type variable
    Variable(Symbol),
    TypedVariable(UniqueName, Intern<Type>),
    Solved(Existential, Intern<Type>),
    Marker(u8),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Element {
    kind: ElementKind,
    // alpha: u32,
    // beta: u32,
}

impl Element {
    #[inline]
    pub fn new_existential(alpha: Existential) -> Self {
        return Self {
            kind: ElementKind::Existential(alpha),
        }
    }

    #[inline]
    pub fn new_typed_variable(name: UniqueName, ty: Intern<Type>) -> Self {
        return Self {
            kind: ElementKind::TypedVariable(name, ty),
        }
    }

    #[inline]
    pub fn new_variable(name: Symbol) -> Self {
        return Self {
            kind: ElementKind::Variable(name),
        }
    }

    #[inline]
    pub fn new_solved(alpha: Existential, ty: Intern<Type>) -> Self {
        return Self {
            kind: ElementKind::Solved(alpha, ty),
        }
    }
}

/// Ordering for elements is determined by their insertion id(s). This gives us
/// a way to insert elements into the middle of the
// impl PartialOrd for Element {
//     fn partial_cmp(&self, other: &Element) -> Option<std::cmp::Ordering> {
//         // If the alpha ordering is not equal, use that
//         if self.alpha != other.alpha {
//             Some(self.alpha.cmp(&other.alpha))
//         } else {
//             Some(self.beta.cmp(&other.beta))
//         }
//     }
// }

// impl Ord for Element {
//     fn cmp(&self, other: &Self) -> std::cmp::Ordering {
//         self.partial_cmp(other).unwrap()
//      }
// }

// impl Element {
//     pub fn fresh(alpha: u32, kind: ElementKind) -> Self {
//         Element { alpha, beta: 0, kind }
//     }
// }

/// An ordered list of type elements, as described in https://arxiv.org/pdf/1306.6032.pdf.
/// We also track scope markers and solved existentials on the side.
///
/// This ordered list is an interested data structure problem. We need to be able to:
///   * Iterate in order
///   * Insert and remove element in the middle
///   * Drop `n` number of elements from the end
///   * Find an element in the list
///   * Iterate over a subset range
///
/// We currently just use a vector for this. It makes it simple to insert and iterate, but is
/// slower for most things.
///
/// Maybe we want a BTreeMap where the order is determined by an `Ord` implementation that can
/// handle inserts.
#[derive(Default)]
pub struct TypeContext {
    pub(crate) elements: Vec<Element>,
    scope_markers: Vec<usize>,
    solved_existentials: HashMap<Existential, Type>,
}

impl TypeContext {
    /// Adds a new element to the ordered type context
    pub(crate) fn add(&mut self, element: Element) {
        self.elements.push(element)
    }
    /// Create a new scope, which will be contain
    /// type context elements that will/can be dropped when
    /// this scope ends
    pub(crate) fn enter_scope(&mut self) {
        let index = self.elements.len();
        self.scope_markers.push(index);
    }

    pub(crate) fn leave_scope(&mut self) {
        let index =
            self.scope_markers.pop().expect("Cant pop the global scope");
        // Clear out items from this scope
        self.elements.truncate(index)
    }

    pub(crate) fn get_annotation(
        &self,
        name: &UniqueName,
    ) -> Option<Intern<Type>> {
        debug!("get_annotation: {:?}", name);
        debug!("{:#?}", self.elements);
        for element in &self.elements {
            match &element.kind {
                ElementKind::TypedVariable(a, ty) if a == name => {
                    return Some(*ty)
                }
                _ => {}
            }
        }
        None
    }

    pub(crate) fn insert_in_place(
        &mut self,
        element: &Element,
        inserts: Vec<Element>,
    ) {
        debug!("insert_in_place: element: {:?}", element);
        debug!("insert_in_place: inserts: {:#?}", inserts);
        if let Some(index) =
            self.elements.iter().position(|elem| elem == element)
        {
            self.elements.splice(index..=index, inserts);
            return;
        }

        debug!("E: {:#?}", self.elements);
        panic!("insert_in_place called with non-existent element")
    }

    pub(crate) fn drop_from_index(&mut self, _index: usize) {
        // ...
    }

    pub(crate) fn get_solved(
        &self,
        alpha: &Existential,
    ) -> Option<Intern<Type>> {
        for element in &self.elements {
            if let ElementKind::Solved(beta, solved) = element.kind {
                if alpha == &beta {
                    return Some(solved);
                }
            }
        }
        None
    }

    pub(crate) fn is_well_formed(&self, _ty: &Type) -> bool {
        true
    }

    pub(crate) fn solve_existential(
        &mut self,
        existential: Existential,
        ty: Intern<Type>,
    ) -> Result<()> {
        debug!("solve_existential: {:?}, {:?}", existential, ty);
        for element in &mut self.elements {
            if let ElementKind::Existential(alpha) = element.kind {
                if alpha == existential {
                    // Mutate it in place
                    *element = Element::new_solved(alpha, ty);
                }
            }
        }
        // Find the element for this existential
        Ok(())
    }

    /// Split a context at and an index. This is used to split
    /// the type context into a left and right context, though
    /// the right context seems unused in the reference implementation
    fn split_at(&self, _index: usize) {}

    fn iterate_to_the_left_of(&self, _index: usize) {}
}
