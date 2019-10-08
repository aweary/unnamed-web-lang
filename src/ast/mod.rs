mod expr;
mod stmt;

use crate::symbol::Symbol;
use crate::typecheck::TKind;

pub use expr::*;
pub use stmt::*;


// The root node of the AST.
#[derive(Debug)]
pub struct Program {
    // The set of all modules the program requires.
    pub modules: Vec<Module>,
    // Arena that stores all the template references.
}

// An individual module. Currently the only way to define
// a module is with a new file, so this maps directly to the filesystem
// layout of the project.
#[derive(Debug)]
pub struct Module {
    pub stmts: Vec<Decl>,
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy)]
pub enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1,
    CONDITIONAL = 2,
    SUM = 3,
    PRODUCT = 4,
    COMPARE = 5,
    PREFIX = 6,
}






