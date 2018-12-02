use ast::*;
use lexer::Lexer;
use token::*;

pub struct Parser {
    pub lexer: Lexer,
    // state: ParserState,
}

impl Parser {
    pub fn module(source: String) -> Self {
        let lexer = Lexer::new(source);
        // let state = ParserState::new();
        Parser { lexer }
    }

    fn peek(&mut self) -> Option<&Token> {
        self.lexer.peek_token()
    }

    pub fn skip_until<F>(&mut self, predicate: F)
    where
        F: Fn(Token) -> bool,
    {
        loop {
            match self.lexer.next_token() {
                Some(token) => {
                    if predicate(token) {
                        return;
                    }
                }
                None => return,
            }
        }
    }

    pub fn skip_assert(&mut self, token: Token) {
        assert_eq!(Some(token), self.lexer.next_token())
    }

    fn read_param_list(&mut self) -> Option<Vec<Param>> {
        match self.lexer.next_token() {
            // If the next token is an opening curly bracket, there
            // are no arguments and we assume this component takes no input
            Some(Token::LCurlyBracket) => None,
            // If the next token is an opening paren, there is
            // an argument list.
            Some(Token::LParen) => {
                let mut params: Vec<Param> = vec![];
                loop {
                    let name = self.read_ident();
                    // TODO this should be an unexpected token error
                    assert_eq!(Some(Token::Colon), self.lexer.next_token());
                    let kind = self.read_ident();
                    params.push(Param::new(name, kind));
                    match self.lexer.next_token() {
                        Some(Token::RParen) => {
                            break;
                            // End of the param list
                        }
                        Some(Token::Comma) => {
                            continue;
                            // There's another paramter
                        }
                        // Anything else is an unexpected token error
                        _ => unreachable!(),
                    }
                }
                Some(params)
            }
            _ => unreachable!("Unexpected token"),
        }
        // Next token should be the opening paren for the argument list
    }

    fn read_ident(&mut self) -> String {
        match self.lexer.next_token() {
            Some(Token::Ident(name)) => name,
            unknown @ _ => unreachable!("Expected identifier, found: {:?}", unknown),
        }
    }

    // Assumes that the current token is Reserved(Component)
    fn read_component_declaration(&mut self) -> Node {
        // Next token should be the identifier for the component definition
        let name = self.read_ident();
        // Read list of all params
        let params = self.read_param_list();
        // Everything from here until the closing bracket is part of the
        // component definition
        let body = self.read_component_body();
        let decl = ComponentDefinition::new(name, params, body);
        Node::ComponentDefinition(decl)
    }

    // A component is composed of a set of expressions/statements.
    fn read_component_body(&mut self) -> Vec<Expression> {
        let mut exprs = vec![];
        loop {
            match self.lexer.next_token() {
                // Statements starting with '<' are assumed to be
                // the beginning of JSX statements.
                Some(Token::LessThan) => {
                    let expr = Expression::JSXElement(self.read_jsx_element());
                    exprs.push(expr);
                    return exprs;
                }
                _ => {
                    continue;
                }
            }
        }
    }

    /**
     * read_expr
     * Reads any expresison, which is something that can evaluate to some value
     */
    fn read_expr(&mut self) -> Expression {
        self.skip_until(|token| token == Token::RCurlyBracket);
        Expression::NumericLiteral(42)
    }

    fn read_jsx_attributes(&mut self) -> Option<Vec<JSXAttribute>> {
        let mut attrs: Vec<JSXAttribute> = vec![];
        loop {
            match self.peek() {
                Some(&Token::Ident(_)) => {
                    println!("peeking an ident");
                    let attr = self.read_jsx_attribute();
                    println!("attr: {:#?}", attr);
                    attrs.push(attr);
                }
                Some(&Token::ForwardSlash) | Some(&Token::GreaterThan) => {
                    break;
                }
                _ => {
                    unreachable!("Unknown token: {:?}", self.peek());
                }
            }
        }
        if attrs.is_empty() {
            None
        } else {
            Some(attrs)
        }
    }

    // fn read_jsx_children(&mut self) -> JSXElement {
    //
    // }

    // Assumes current token is '<' for an opening JSX tag
    fn read_jsx_element(&mut self) -> JSXElement {
        let tag_name = self.read_ident();
        let attributes = self.read_jsx_attributes();
        let opening_element = JSXOpeningElement::new(tag_name.clone(), attributes, false);
        let children = self.read_jsx_children();
        let closing_element = JSXClosingElement::new(tag_name);
        JSXElement::new(opening_element, closing_element, children)
    }

    fn read_jsx_children(&mut self) -> Box<JSXChildren> {
        println!("read_jsx_children : {:?}", self.lexer.ch());
        let text = String::from("Hello, world");
        Box::new(JSXChildren::JSXText(text))
    }

    /**
     * Reads the expression part of a JSX attribute.
     */
    fn read_jsx_attribute_value(&mut self) -> JSXAttributeValue {
        // An explicit assignment. The assigned value can
        // either be a string literal or an identifier in curly brackets
        match self.lexer.next_token() {
            Some(Token::String(literal)) => JSXAttributeValue::StringLiteral(literal),
            Some(Token::LCurlyBracket) => {
                let expr = self.read_expr();
                JSXAttributeValue::Expression(expr)
            }
            token @ _ => unreachable!("Unknown token: {:?}", token),
        }
    }

    /**
     * read_jsx_attr
     *
     * A JSX Attribute comes in two forms:
     *
     * 1. An explicit attribute where the value is passed
     *    in inline (<div id="1" />)
     * 2. An implicit attribute where we assume the attribute
     *    name is a reference to some identifier
     *    (<div id>)
     */
    fn read_jsx_attribute(&mut self) -> JSXAttribute {
        let name = self.read_ident();

        let foo = match self.peek() {
            Some(&Token::Assign) => {
                self.skip_assert(Token::Assign);
                let attribute_value = self.read_jsx_attribute_value();
                JSXAttribute::new(name, attribute_value)
            }
            Some(&Token::Ident(_)) | Some(&Token::ForwardSlash) | Some(&Token::GreaterThan) => {
                let attr_ident = Identifier::new(name.clone());
                let attr_ident_expr = Expression::Identifer(attr_ident);
                let attr_value = JSXAttributeValue::Expression(attr_ident_expr);
                JSXAttribute::new(name, attr_value)
            }
            _ => {
                unreachable!();
            }
        };
        foo
        // println!("read_jsx_attribute:name {:?}", name);
        // match self.lexer.next_token() {
        //     // Explicit assignment
        //     Some(Token::Assign) => {
        //         let attr_value = self.read_jsx_attribute_value();
        //         JSXAttribute::new(name, attr_value)
        //     }
        //     // Shorthand form
        //     Some(Token::Ident(_)) | Some(Token::ForwardSlash) | Some(Token::GreaterThan) => {
        //         println!("shorthand form, creating the identifier and stuff");
        //         let attr_ident = Identifier::new(name.clone());
        //         let attr_ident_expr = Expression::Identifer(attr_ident);
        //         let attr_value = JSXAttributeValue::Expression(attr_ident_expr);
        //         JSXAttribute::new(name, attr_value)
        //     }
        //     token @ _ => unreachable!("Unknown character: {:?}", token),
        // }
    }

    fn read_type_def(&mut self) -> Node {
        // Next token is the identifier for the type definition.
        let name = self.read_ident();
        let def = TypeDefinition::new(name);
        // Just read everything until we reach the end for now...
        loop {
            if self.lexer.next_token() == Some(Token::RCurlyBracket) {
                break;
            }
        }
        Node::TypeDefinition(def)
    }

    /**
     * Inside a module, top level nodes are currently restricted to component
     * and function definitions. This means you cannot have other expressions
     * that are executed when the module is evalulated. All components and
     * functions must be lazily evaluated.
     */
    pub fn next_node(&mut self) -> Node {
        let token = self.lexer.next_token().unwrap();
        match token {
            Token::Reserved(ReservedWord::Component) => self.read_component_declaration(),
            Token::Reserved(ReservedWord::Type) => self.read_type_def(),
            _ => Node::Text,
        }
    }
}
