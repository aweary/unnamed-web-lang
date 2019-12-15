use serde::{Deserialize, Serialize};
use syntax::ast;
use syntax::symbol::Symbol;

use generational_arena::{Arena, Index};

/// Index for an expression IR node
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
struct ExprId(pub Index);

/// Identifies a unique definition
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
struct DefId(pub Index);

/// Identifies a unique module
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
struct ModuleId {
    // ...
}

struct ExprArena {
    map: Arena<ast::Expr>,
}

impl ExprArena {
    pub fn alloc(&mut self, expr: ast::Expr) -> ExprId {
        ExprId(self.map.insert(expr))
    }
}

/// IR for a component definition.
#[derive(Clone, Debug, Serialize, Deserialize)]
struct ComponentDef {
    // ...
}

/// Reference an HTML element tag name
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
struct ElementReference(u32);

// Bytecode-like instruction for template IR
#[derive(Clone, Debug, Serialize, Deserialize, Hash, Eq, PartialEq)]
enum TemplateInst {
    // Create a new HTML element
    PushHostElement(ElementReference),
    // Pop the topmost HTML element off of the stack
    PopHostElement,
    // Insert static text into the topmost HTML element on the stack
    StaticText(Symbol),
    DynamicText(ExprId),
}
