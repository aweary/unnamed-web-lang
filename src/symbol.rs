use std::cell::RefCell;
use std::fmt;
use std::num::NonZeroU32;
use string_interner::StringInterner;
pub use string_interner::Symbol as SymbolTrait;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol(NonZeroU32);

impl SymbolTrait for Symbol {
    fn from_usize(val: usize) -> Self {
        Symbol(NonZeroU32::new((val + 1) as u32).unwrap_or_else(|| {
            unreachable!("Should never fail because `val + 1` is nonzero and `<= u32::MAX`")
        }))
    }
    fn to_usize(self) -> usize {
        (self.0.get() as usize) - 1
    }
}

// Display the interned string for fmt::Debug
impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        SYMBOLS.with(|symbols| {
            let interner = symbols.interner.borrow();
            let string = interner.resolve(*self).unwrap();
            write!(f, "\"s#{}\"", string)
        })
    }
}

#[derive(Debug)]
pub struct Symbols {
    interner: RefCell<StringInterner<Symbol>>,
}

impl Symbols {
    pub fn new() -> Symbols {
        Symbols {
            interner: RefCell::new(StringInterner::new()),
        }
    }
}

thread_local!(pub static SYMBOLS: Symbols = Symbols::new());

#[inline]
pub fn symbol(name: &str) -> Symbol {
    SYMBOLS.with(|symbols| {
        let mut interner = symbols.interner.borrow_mut();
        interner.get_or_intern(name)
    })
}
