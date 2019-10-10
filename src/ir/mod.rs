mod cfg;
/**
 * The IR format used for almost all checks or additional passes
 * in the compiler. We lower the AST to this IR format and then
 * use this to codegen modules.
 */
mod scope;

use std::num::NonZeroU32;

use crate::ast::{ExprId, TemplateId};
use crate::symbol::Symbol;
use cfg::CFG;
use scope::Scope;


pub fn resolve(name: Symbol, scope: &Scope) -> Option<ExprId> {
    None
    // ...
}

/**
 * Index into an arena that stores all the templates used across
 * all components in a program.
 */
pub struct DefId(NonZeroU32);

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy)]
pub struct StaticTemplateId(pub u32);

/**
 * The two types of templates. A static template is one that renders completely
 * static content, meaning that we know at compile time what its output will be.
 *
 * A dynamic template requires some kind of interpolation.
 */
#[derive(Debug, Hash)]
pub enum TemplateKind {
    Static,
    Dynamic,
}

/**
 * The template IR is modeled similar to a stack-based bytecode format. This
 * contains all the possible instructions we'd so for construction a template.
 */
#[derive(Debug, Hash, PartialEq, Eq)]
pub enum TemplateInstr {
    // The start of a new element
    PushElement(Symbol),
    // A attribute on the most recently created element, static value.
    SetStaticAttribute(Symbol, Symbol),
    // A attribute on the most recently created element, dynamic value.
    SetDynamicAttribute(Symbol, ExprId),
    // Append text to the most recently created element
    InsertText(Symbol),
    EmbedExpr(ExprId),
    // A static subtree that we've hoisted out.
    EmbedStaticTemplate(StaticTemplateId),
    // Pop an element off of th stack
    PopElement,
    // ...
}

pub type TemplateInstrList = Vec<TemplateInstr>;

#[derive(Debug, Hash)]
pub struct Template {
    pub instr: TemplateInstrList,
    pub kind: TemplateKind,
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
