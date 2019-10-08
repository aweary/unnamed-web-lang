mod cfg;
/**
 * The IR format used for almost all checks or additional passes
 * in the compiler. We lower the AST to this IR format and then
 * use this to codegen modules.
 */
mod scope;

use std::num::NonZeroU32;

use crate::symbol::Symbol;
use cfg::CFG;
use scope::Scope;

pub struct Env {}

pub fn resolve(name: Symbol, scope: &Scope) -> Option<ExprId> {
    None
    // ...
}

// Basic types that can be composed together
enum Ty {
  Number,
  Str,
  Bool,
}

/**
 * Index into an arena that stores all the templates used across
 * all components in a program.
 */
pub struct TemplateId(NonZeroU32);

pub struct ExprId(NonZeroU32);
pub struct DefId(NonZeroU32);

/**
 * The two types of templates. A static template is one that renders completely
 * static content, meaning that we know at compile time what its output will be.
 *
 * A dynamic template requires some kind of interpolation.
 */
pub enum TemplateKind {
    Static,
    Dynamic,
}

/**
 * The template IR is modeled similar to a stack-based bytecode format. This
 * contains all the possible instructions we'd so for construction a template.
 */
pub enum TemplateInstr {
    CreateElement(Symbol),
    SetStaticAttribute(Symbol, Symbol),
    InsertText(Symbol),
    // ...
}

pub struct Template {
    instr: Vec<TemplateInstr>,
    kind: TemplateKind,
}

// A component definition
pub struct Component {
    name: Symbol,
    templates: Vec<TemplateId>,
}

pub struct Block {
    // Does this block allow
    allow_definitions: bool,
    stmts: Vec<DefId>,
}
