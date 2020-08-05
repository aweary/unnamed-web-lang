use common::unique_name::UniqueName;
use diagnostics::ParseResult as Result;
use ty::{Existential, Type, Variable};

use internment::Intern;
use log::debug;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElementKind {
    /// An existential type
    Existential(Existential),
    /// An unsolved type variable
    TypedVariable(UniqueName, Intern<Type>),
    Solved(Existential, Intern<Type>),
    Marker(Existential),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Element {
    kind: ElementKind,
}

impl Element {
    #[inline]
    pub fn new_variable(alpha: Variable) -> Self {
        return Self {
            kind: ElementKind::Variable(alpha),
        };
    }

    #[inline]
    pub fn new_existential(alpha: Existential) -> Self {
        return Self {
            kind: ElementKind::Existential(alpha),
        };
    }

    #[inline]
    pub fn new_marker(alpha: Existential) -> Self {
        return Self {
            kind: ElementKind::Marker(alpha),
        };
    }

    #[inline]
    pub fn new_typed_variable(name: UniqueName, ty: Intern<Type>) -> Self {
        return Self {
            kind: ElementKind::TypedVariable(name, ty),
        };
    }

    #[inline]
    pub fn new_solved(alpha: Existential, ty: Intern<Type>) -> Self {
        return Self {
            kind: ElementKind::Solved(alpha, ty),
        };
    }
}

/// An ordered list of type elements, as described in [this paper](https://arxiv.org/pdf/1306.6032.pdf).
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
    // solved_existentials: HashMap<Existential, Type>,
}

impl TypeContext {
    /// Adds a new element to the ordered type context
    pub(crate) fn add(&mut self, element: Element) {
        // debug!("adding context element: {:?}", element);
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
        for element in &self.elements {
            match &element.kind {
                ElementKind::TypedVariable(a, ty) if a == name => {
                    debug!("get_annotation: {:?} is {}", name, ty);
                    return Some(*ty);
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

    pub(crate) fn drop(&mut self, element: &Element) {
        if let Some(index) = self.elements.iter().position(|ele| ele == element)
        {
            let mut eles = self.elements.clone();
            eles.truncate(index);
        }
        // ...
    }

    pub(crate) fn get_solved(
        &self,
        alpha: Existential,
    ) -> Option<Intern<Type>> {
        for element in &self.elements {
            if let ElementKind::Solved(beta, solved) = element.kind {
                if alpha == beta {
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
                    return Ok(());
                }
            }
        }
        // Find the element for this existential
        panic!(
            "Attempted to solve '{:?}' which doesnt exist in context",
            existential
        )
    }

    /// Split a context at and an index. This is used to split
    /// the type context into a left and right context, though
    /// the right context seems unused in the reference implementation
    fn split_at(&self, _index: usize) {}

    fn iterate_to_the_left_of(&self, _index: usize) {}
}
