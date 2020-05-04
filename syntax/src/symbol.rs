use data_structures::scope_map::Reference;
use serde::{Deserialize, Serialize};

use std::fmt::{Debug, Error, Formatter};
use std::result::Result;

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Symbol {
    // Temporary representation, we will eventually intern the string
    // with a global interner like rustc, but for now this helps debugging
    unstable_source: String,
}

impl Reference for Symbol {}

impl Symbol {
    pub fn intern(s: &str) -> Symbol {
        // ...
        Symbol {
            unstable_source: String::from(s),
        }
    }

    pub fn as_str(&self) -> &str {
        &self.unstable_source
    }
}

// Symbols are used to represent all literal values in the AST,
// which needs to be converted to the correct representation in
// the IR. This is a somewhat-unsafe way to do it via Symbol itself.
// The Ident AST struct implements a safer conversion on top of this.
macro_rules! symbol_into {
    ($into:ident) => {
        impl Into<$into> for Symbol {
            fn into(self) -> $into {
                self.as_str().parse::<$into>().unwrap()
            }
        }
    };
}

// TODO how to handle floating point numbers?
symbol_into!(u32);
symbol_into!(bool);
symbol_into!(String);

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.unstable_source)
    }
}
