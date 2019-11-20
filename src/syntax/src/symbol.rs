use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Serialize, Deserialize, PartialEq, Eq, Hash)]
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
