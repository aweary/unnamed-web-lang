use serde::{Deserialize, Serialize};

use std::fmt::{Debug, Error, Formatter};
use std::result::Result;

#[derive(Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct Symbol {
    // Temporary representation, we will eventually intern the string
    // with a global interner like rustc, but for now this helps debugging
    unstable_source: String,
}

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

impl Debug for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", self.unstable_source)
    }
}
