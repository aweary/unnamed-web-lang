use crate::ast::{
    Body, Expression, Identifier, JSXAttribute, JSXChild, JSXClosingElement, JSXElement,
    JSXOpeningElement, JSXText, Module, Operator, Param, Precedence, Statement,
};
use crate::error::ParseError;
use crate::lexer::Lexer;
use crate::token::{Keyword, Token, TokenKind};

type Result<T> = std::result::Result<T, ParseError>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

macro_rules! keyword {
    ($id:ident) => {
        TokenKind::Keyword(Keyword::$id)
    };
}

impl<'a> Parser<'a> {
    pub fn parse<'s>(source: &'s String) -> Result<Module> {
        let lexer = Lexer::new(&source);
        let mut parser = Parser { lexer };
        // parse_module
        let stmts = parser.stmt_list()?;
        let body = Body::new(stmts);
        let module = Module::new(body);
        Ok(module)
    }

    // move to trait?
    fn peek(&mut self) -> Result<&Token> {
        match self.lexer.peek() {
            Ok(token) => Ok(token),
            // TODO how to manage this? want lexer to be peekable but it can also report errors?
            // Or should lexing fail early? It can't if we do lexing lazily
            Err(_) => Err(ParseError::LexError),
        }
    }

    fn expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.expr(0)?;
        Ok(Statement::ExpressionStatement(expr))
    }

    fn return_stmt(&mut self) -> Result<Statement> {
        self.expect(TokenKind::Keyword(Keyword::Return))?;
        let expr = self.expr(0)?;
        Ok(Statement::Return(expr))
    }

    fn if_stmt(&mut self) -> Result<Statement> {
        self.expect(keyword!(If))?;
        let test = self.expr(0)?;
        // TODO self.read_block()?;
        self.expect(TokenKind::LBrace)?;
        let consequent_stmts = self.stmt_list()?;
        self.expect(TokenKind::RBrace)?;
        let mut alternate = None;
        if self.peek()?.kind == keyword!(Else) {
            self.expect(keyword!(Else))?;
            self.expect(TokenKind::LBrace)?;
            let stmts = self.stmt_list()?;
            self.expect(TokenKind::RBrace)?;
            alternate = Some(Body::new(stmts));
        }
        Ok(Statement::If {
            test,
            consequent: Body::new(consequent_stmts),
            // TODO
            alternate,
        })
    }

    fn stmt(&mut self) -> Result<Statement> {
        match &self.peek()?.kind {
            TokenKind::Keyword(keyword) => match keyword {
                Keyword::Let => self.var_decl(),
                Keyword::Func => self.func_decl(),
                Keyword::Return => self.return_stmt(),
                Keyword::If => self.if_stmt(),
                // Else cannot be the start of a statement
                Keyword::Else => Err(ParseError::UnexpectedToken(self.next()?)),
            },
            // ExpressionStatement
            TokenKind::Ident(_) => self.expr_stmt(),
            _ => {
                let token = self.lexer.next().unwrap();
                return Err(ParseError::UnexpectedToken(token));
            }
        }
    }

    fn stmt_list(&mut self) -> Result<Vec<Statement>> {
        let mut statements = vec![];
        while !self.peek()?.follow_stmt_list() {
            statements.push(self.stmt()?);
            if self.peek()?.kind == TokenKind::Semi {
                // eat
                self.next()?;
            }
        }
        Ok(statements)
    }

    // fn decl(&mut self) -> Result<Declaration> {
    //     // ...
    //     Err(ParseError::UnexpectedEOF);
    // }

    fn next(&mut self) -> Result<Token> {
        match self.lexer.next() {
            Ok(token) => Ok(token),
            Err(_) => Err(ParseError::LexError),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.next()?;
        if token.kind != kind {
            println!("Expected {:?}, got {:?}", kind, token.kind);
            Err(ParseError::UnexpectedToken(token))
        } else {
            Ok(token)
        }
    }

    fn matches(&mut self, kind: TokenKind) -> Result<bool> {
        if self.peek()?.kind == kind {
            // eat
            self.expect(kind)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn ident(&mut self) -> Result<Identifier> {
        // TODO how to deduplicate this w/ expr?
        let token = self.next()?;
        match token.kind {
            TokenKind::Ident(name) => {
                let id = Identifier(name);
                Ok(id)
            }
            _ => {
                println!("unknown ident");
                Err(ParseError::UnexpectedToken(token))
            }
        }
    }

    fn jsx_children(&mut self) -> Result<Option<Vec<JSXChild>>> {
        let mut children: Vec<JSXChild> = vec![];
        loop {
            // TODO make this peek so that jsx_close_element can expect
            // LessThan
            match self.next()?.kind {
                TokenKind::LessThan => {
                    if self.peek()?.kind == TokenKind::Div {
                        // Closing element, end of children
                        return Ok(Some(children));
                    }
                    let element = self.jsx_element()?;
                    let child = JSXChild::JSXElement(Box::new(element));
                    children.push(child);
                }
                TokenKind::LBrace => {
                    let expr = self.expr(0)?;
                    self.expect(TokenKind::RBrace)?;
                    let child = JSXChild::JSXExpression(expr);
                    children.push(child);
                }
                TokenKind::JSXText(text) => {
                    let jsx_text = JSXText::new(text);
                    let child = JSXChild::JSXText(jsx_text);
                    children.push(child);
                }
                _ => {
                    if children.is_empty() {
                        return Ok(None);
                    } else {
                        return Ok(Some(children));
                    }
                }
            }
        }
    }

    fn jsx_close_element(&mut self) -> Result<Option<JSXClosingElement>> {
        // self.expect(TokenKind::LessThan)?;
        self.expect(TokenKind::Div)?;
        let ident = self.ident()?;
        self.expect(TokenKind::GreaterThan)?;
        Ok(Some(JSXClosingElement::new(ident)))
    }

    fn jsx_attribute(&mut self) -> Result<JSXAttribute> {
        let name = self.ident()?;
        self.expect(TokenKind::Equals);
        let value = match self.peek()?.kind {
            TokenKind::StringLiteral(_) => {
                let string_token = self.next()?;
                Expression::StringLiteral(string_token)
            }
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                let expr = self.expr(0)?;
                self.expect(TokenKind::RBrace)?;
                expr
            }
            _ => {
                return Err(ParseError::UnexpectedToken(self.next()?))
            }
        };
        Ok(JSXAttribute::new(name, Box::new(value)))
    }

    fn jsx_attributes(&mut self) -> Result<Option<Vec<JSXAttribute>>> {
        let mut attributes : Vec<JSXAttribute> = vec![];
        loop {
        match self.peek()?.kind {
            TokenKind::GreaterThan => {
                if attributes.is_empty() {
                    return Ok(None)
                } else {
                    return Ok(Some(attributes))
                }
            }
            TokenKind::Ident(_) => {
                let attr = self.jsx_attribute()?;
                attributes.push(attr);
            }
            _ => return Err(ParseError::UnexpectedToken(self.next()?))
        }
        }
    }

    fn jsx_element(&mut self) -> Result<JSXElement> {
        let tag_name = self.ident()?;
        let attrs = self.jsx_attributes()?;
        // TODO don't clone tag_name
        let open = JSXOpeningElement::new(tag_name.clone(), attrs);
        // TODO attributes,  move to jsx_open_element;
        self.expect(TokenKind::GreaterThan)?;
        let children = self.jsx_children()?;
        let close = self.jsx_close_element()?;
        if false {
            // TODO self closing elements]
            // JSXClosingElement::new(tag_name.clone());
        }
        let elem = JSXElement::new(open, close, children);
        println!("elem {:?}", elem);
        Ok(elem)
    }

    fn prefix_expr(&mut self) -> Result<Expression> {
        let token = self.next()?;
        match token.kind {
            // Literals
            TokenKind::NumericLiteral(_) => Ok(Expression::NumericLiteral(token)),
            TokenKind::StringLiteral(_) => Ok(Expression::StringLiteral(token)),
            TokenKind::Ident(name) => Ok(Expression::Identifier(Identifier(name))),
            // JSX Expresion
            TokenKind::LessThan => Ok(Expression::JSXExpression(self.jsx_element()?)),
            // Unary operators
            TokenKind::Plus => {
                let operand = self.expr(Precedence::PREFIX as u32)?;
                Ok(Expression::UnaryExpression {
                    operator: Operator::Plus,
                    expr: Box::new(operand),
                })
            }
            // TODO dedupe
            TokenKind::Minus => {
                let operand = self.expr(Precedence::PREFIX as u32)?;
                Ok(Expression::UnaryExpression {
                    operator: Operator::Minus,
                    expr: Box::new(operand),
                })
            }
            // Group
            TokenKind::LParen => {
                let expr = self.expr(Precedence::GROUP as u32)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            _ => Err(ParseError::UnexpectedToken(token)),
        }
    }

    fn cond_expr(&mut self, test: Expression) -> Result<Expression> {
        self.expect(TokenKind::Question)?;
        let test = Box::new(test);
        let consequent = Box::new(self.expr(0)?);
        self.expect(TokenKind::Colon)?;
        let alternate = Box::new(self.expr((Precedence::CONDITIONAL as u32) - 1)?);
        Ok(Expression::ConditionalExpression {
            test,
            consequent,
            alternate,
        })
    }

    fn binary_expr(&mut self, left: Expression) -> Result<Expression> {
        // TODO move this out
        let (operator, precedence) = match self.peek()?.kind {
            TokenKind::Plus => (Operator::Plus, Precedence::SUM as u32),
            TokenKind::Minus => (Operator::Minus, Precedence::SUM as u32),
            TokenKind::Mul => (Operator::Mul, Precedence::PRODUCT as u32),
            TokenKind::Div => (Operator::Div, Precedence::PRODUCT as u32),
            TokenKind::LessThan => (Operator::LessThan, Precedence::COMPARE as u32),
            TokenKind::GreaterThan => (Operator::GreaterThan, Precedence::COMPARE as u32),
            TokenKind::DblEquals => (Operator::Equals, Precedence::COMPARE as u32),
            _ => return Err(ParseError::UnexpectedToken(self.next()?)),
        };
        // TODO fix this
        // eat
        self.next()?;
        // TODO handle other operators
        let right = self.expr(precedence)?;
        Ok(Expression::BinaryExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn infix_expr(&mut self, left: Expression) -> Result<Expression> {
        match self.peek()?.kind {
            // ConditionalExpression
            TokenKind::Question => self.cond_expr(left),
            // BinaryExpression
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Div
            | TokenKind::Mul
            | TokenKind::LessThan
            | TokenKind::DblEquals
            | TokenKind::GreaterThan => self.binary_expr(left),
            _ => Ok(left),
        }
    }

    fn peek_precedence(&mut self) -> Result<u32> {
        Ok(match self.peek()?.kind {
            TokenKind::Equals => Precedence::ASSIGNMENT as u32,
            TokenKind::Question => Precedence::CONDITIONAL as u32,
            TokenKind::Plus => Precedence::SUM as u32,
            TokenKind::Minus => Precedence::SUM as u32,
            TokenKind::Mul => Precedence::PRODUCT as u32,
            TokenKind::Div => Precedence::PRODUCT as u32,
            TokenKind::DblEquals => Precedence::COMPARE as u32,
            TokenKind::LessThan | TokenKind::GreaterThan => Precedence::COMPARE as u32,
            _ => 0,
        })
    }

    fn expr(&mut self, precedence: u32) -> Result<Expression> {
        // todo error handling
        let mut expr = self.prefix_expr()?;
        while precedence < self.peek_precedence()? {
            expr = self.infix_expr(expr)?;
        }
        Ok(expr)
    }

    fn var_decl(&mut self) -> Result<Statement> {
        // This should handle const or othher variable decalaration keywords
        // we might add later.
        self.expect(keyword!(Let))?;
        let ident = self.ident()?;
        self.expect(TokenKind::Equals)?;
        let expr = self.expr(0)?;
        let statement = Statement::VariableDeclaration {
            id: ident,
            value: expr,
        };
        Ok(statement)
    }

    fn param(&mut self) -> Result<Param> {
        let name = self.ident()?;
        self.expect(TokenKind::Colon)?;
        // annotations are current not optional
        let annotation = self.ident()?;
        // this allows trailing commas
        self.matches(TokenKind::Comma)?;
        Ok(Param::new(name, annotation))
    }

    fn param_list(&mut self) -> Result<Vec<Param>> {
        let mut params = vec![];
        self.expect(TokenKind::LParen)?;
        loop {
            match self.peek()?.kind {
                TokenKind::RParen => {
                    self.expect(TokenKind::RParen)?;
                    return Ok(params);
                }
                TokenKind::Ident(_) => params.push(self.param()?),
                _ => return Err(ParseError::UnexpectedToken(self.next()?)),
            }
        }
    }

    fn func_decl(&mut self) -> Result<Statement> {
        self.expect(keyword!(Func))?;
        let fn_name = self.ident()?;
        let params = self.param_list()?;
        // Return types are not optional.
        self.expect(TokenKind::Colon)?;
        let return_type = self.ident()?;
        // Block
        self.expect(TokenKind::LBrace)?;
        let stmts = self.stmt_list()?;
        self.expect(TokenKind::RBrace)?;

        Ok(Statement::FunctionDeclaration {
            name: fn_name,
            body: Body::new(stmts),
            params,
            return_type,
        })
    }
}
