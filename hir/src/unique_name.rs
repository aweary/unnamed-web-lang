use lazy_static::lazy_static;

use std::cell::RefCell;
use std::fmt;
use std::sync::Mutex;

lazy_static! {
    static ref UNIQUE_NAME_COUNTER: Mutex<RefCell<u32>> =
        Mutex::new(RefCell::new(0));
}

#[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct UniqueName(u32);

impl UniqueName {
    pub fn new() -> Self {
        let id = UNIQUE_NAME_COUNTER
            .lock()
            .unwrap()
            .replace_with(|id| *id + 1);
        UniqueName(id)
    }
}

impl fmt::Debug for UniqueName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0)
    }
}
