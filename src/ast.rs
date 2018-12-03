#[derive(Debug)]
pub struct Param {
    name: String,
    kind: String,
}

impl Param {
    pub fn new(name: String, kind: String) -> Self {
        Param { name, kind }
    }
}

/**
 * The definition of a component. Similar to a function definition, but with
 * stricter semantics and requirements.
 *
 * @example
 * component Button(label: string) {
 *   <button>{label}</button>
 * }
 *
 */
#[derive(Debug)]
pub struct ComponentDefinition {
    // Name of the component
    name: String,
    // List of its parameters, which must by typed
    params: Option<Vec<Param>>,
    // The content of the component
    body: Vec<Expression>,
}

impl ComponentDefinition {
    pub fn new(name: String, params: Option<Vec<Param>>, body: Vec<Expression>) -> Self {
        ComponentDefinition { name, params, body }
    }
}

#[derive(Debug)]
pub struct TypeDefinition {
    name: String,
}

impl TypeDefinition {
    pub fn new(name: String) -> Self {
        TypeDefinition { name }
    }
}

#[derive(Debug)]
pub struct Identifier {
    name: String,
}

impl Identifier {
    pub fn new(name: String) -> Self {
        Identifier { name }
    }
}

#[derive(Debug)]
pub struct JSXOpeningElement {
    tag_name: String,
    attributes: Option<Vec<JSXAttribute>>,
    self_closing: bool,
}

impl JSXOpeningElement {
    pub fn new(
        tag_name: String,
        attributes: Option<Vec<JSXAttribute>>,
        self_closing: bool,
    ) -> Self {
        JSXOpeningElement {
            tag_name,
            attributes,
            self_closing,
        }
    }
}

#[derive(Debug)]
pub struct JSXClosingElement {
    tag_name: String,
}

impl JSXClosingElement {
    pub fn new(tag_name: String) -> Self {
        JSXClosingElement { tag_name }
    }
}

#[derive(Debug)]
pub struct JSXAttribute {
    name: String,
    value: JSXAttributeValue,
}

impl JSXAttribute {
    pub fn new(name: String, value: JSXAttributeValue) -> Self {
        JSXAttribute { name, value }
    }
}

#[derive(Debug)]
pub struct JSXElement {
    opening_element: JSXOpeningElement,
    closing_element: Option<JSXClosingElement>,
    children: Option<Vec<Box<JSXChildren>>>,
}

impl JSXElement {
    pub fn new(
        opening_element: JSXOpeningElement,
        closing_element: Option<JSXClosingElement>,
        children: Option<Vec<Box<JSXChildren>>>,
    ) -> Self {
        JSXElement {
            opening_element,
            closing_element,
            children,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    NumericLiteral(i64),
    StringLiteral(String),
    Identifer(Identifier),
    JSXElement(JSXElement),
}

#[derive(Debug)]
pub enum JSXAttributeValue {
    StringLiteral(String),
    Expression(Expression),
}

#[derive(Debug)]
pub enum JSXChildren {
    JSXText(String),
    JSXExpressionContainer(Expression),
    JSXElement(JSXElement),
}

// pub enum Expression {
//     JSXElement,
//     JSXOpeningElement,
//     JSXClosingElement,
//     JSXAttribute,
// }

#[derive(Debug)]
pub enum Node {
    ComponentDefinition(ComponentDefinition),
    TypeDefinition(TypeDefinition),
    // Identifier,
    // OpeningElement,
    // ClosingElement,
    Text,
    // ExpressionStatement,
    // StateCellDeclaration,
}
