pub type Heap = u32;

use internment::Intern;
use crate::ty::InternType;

use data_structures::MultiSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Effect {
    row: MultiSet<EffectConstant>,
}



#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum EffectConstant {
    /// Exceptions, of some type
    Exn(InternType),
    /// Divergence
    Div,
    /// Heap operations over some heap
    St(Heap),
    // State
    State
}


/// Most constants compare directly. The only exception in our system is
/// the state effect where `st<h1> != st<h2>` does *not* hold even if `h1 != h2`
// impl PartialEq for EffectConstant {
//     fn eq(&self, other: &EffectConstant) -> bool {
//         match (self, other) {
//             (EffectConstant::St(_), EffectConstant::St(_)) => {
//                 // TODO I don't know if this is what the constraint means?
//                 true
//             }
//             _ => self == other
//         }
//     }
// }

// impl Eq for EffectConstant {}

#[derive(Debug, Clone, Hash, PartialEq)]
pub struct EffectType {
    /// Effects are defined as a row of effect labels `l`.
    /// Equivalence between effects
    row: Vec<EffectConstant>,
    /// The kind system ensures that an effect is always either a
    /// closed effect or open effect
    is_open: bool,
}

impl Default for EffectType {
    fn default() -> Self {
        EffectType { row: vec![], is_open: false }
    }
}

impl EffectType {
    pub fn add(&mut self, effect: EffectConstant) {
        self.row.push(effect)
    }

    pub fn extend(&mut self, effect: &EffectType) {
        self.row.extend(effect.row.clone());
        // println!("ROW {:?}", self.row);
        // panic!()
    }
}


/// Defines the equivalence relation between effect types, where we consider
/// effects equivlant regardless of the order of the effect constants.
/// Effect rows *do* allow duplicate labels where `<exn, exn>` is allowed
/// and not equal to `<exn>`
// impl PartialEq for EffectType {
//     fn eq(&self, other: &EffectType) -> bool {
//         // EQ-REFL and by extension EQ-TRANS
//         if self == other {
//             return true;
//         }
//         let a = &self.row;
//         let b = &other.row;
//         a == b
//     }
// }

impl Eq for EffectType {}