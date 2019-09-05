use crate::token::Token;

// TODO make serializable w/ serde
#[derive(Debug)]
pub struct Module {
    pub body: Body,
}

impl Module {
    pub fn new(body: Body) -> Self {
        Module { body }
    }
}

#[derive(Debug)]
pub struct Body {
    pub stmts: Vec<Statement>,
}

impl Body {
    pub fn new(stmts: Vec<Statement>) -> Self {
        Body { stmts }
    }
}

#[derive(Debug)]
pub struct Param {
    name: Identifier,
    annotation: Identifier,
}

impl Param {
    pub fn new(name: Identifier, annotation: Identifier) -> Self {
        Param { name, annotation }
    }
}

#[derive(Debug)]
pub enum Statement {
    VariableDeclaration {
        id: Identifier,
        value: Expression,
    },
    FunctionDeclaration {
        name: Identifier,
        body: Body,
        params: Vec<Param>,
        return_type: Identifier,
    },
    ExpressionStatement(Expression),
    Return(Expression),
    // TODO call this Block instead of Body?
    // Block(Body),
    If {
        test: Expression,
        consequent: Body,
        alternate: Option<Body>,
    },
}

#[derive(Debug)]
pub struct JSXAttribute {
    name: Identifier,
    value: Box<Expression>,
}

impl JSXAttribute {
    pub fn new(name: Identifier, value: Box<Expression>) -> Self {
        JSXAttribute { name, value }
    }
}

#[derive(Debug)]
pub struct JSXOpeningElement {
    name: Identifier,
    attrs: Option<Vec<JSXAttribute>>,
}

impl JSXOpeningElement {
    #[inline]
    pub fn new(name: Identifier, attrs: Option<Vec<JSXAttribute>>) -> Self {
        JSXOpeningElement { name, attrs }
    }
}

#[derive(Debug)]
pub struct JSXClosingElement {
    name: Identifier,
}

impl JSXClosingElement {
    #[inline]
    pub fn new(name: Identifier) -> Self {
        JSXClosingElement { name }
    }
}

#[derive(Debug)]
pub struct JSXText {
    text: String,
}

impl JSXText {
    pub fn new(text: String) -> Self {
        JSXText { text }
    }
}

#[derive(Debug)]
pub enum JSXChild {
    JSXText(JSXText),
    JSXElement(Box<JSXElement>),
    JSXExpression(Expression),
}

#[derive(Debug)]
pub struct JSXElement {
    open: JSXOpeningElement,
    close: Option<JSXClosingElement>,
    children: Option<Vec<JSXChild>>,
}

impl JSXElement {
    #[inline]
    pub fn new(
        open: JSXOpeningElement,
        close: Option<JSXClosingElement>,
        children: Option<Vec<JSXChild>>,
    ) -> Self {
        JSXElement {
            open,
            close,
            children,
        }
    }
}

#[derive(Debug)]
pub struct MatchArm {
    test: Box<Expression>,
    consequent: Box<Expression>,
}

impl MatchArm {
    pub fn new(test: Expression, consequent: Expression) -> Self {
        MatchArm { test: Box::new(test), consequent: Box::new(consequent) }
    } 
}

#[derive(Debug)]
pub enum Expression {
    // TODO these literals shouldnt reference the tokens
    NumericLiteral(Token),
    StringLiteral(Token),
    Identifier(Identifier),
    UnaryExpression {
        operator: Operator,
        expr: Box<Expression>,
    },
    BinaryExpression {
        operator: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    ConditionalExpression {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Box<Expression>,
    },
    MemberExpression {
        object: Box<Expression>,
        property: Identifier,
    },
    CallExpression {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    MatchExpression {
        discriminant: Box<Expression>,
        cases: Vec<MatchArm>,
    },
    JSXExpression(JSXElement),
}

#[derive(Debug, Clone)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub enum Precedence {
    GROUP = 0,
    ASSIGNMENT = 1,
    CONDITIONAL = 2,
    SUM = 3,
    PRODUCT = 4,
    COMPARE = 5,
    PREFIX = 6,
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
    LessThan,
    GreaterThan,
    Equals,
}
