use crate::ast::{
    Body, Expression, Identifier, JSXAttribute, JSXChild, JSXClosingElement, JSXElement,
    JSXOpeningElement, JSXText, MatchArm, Module, Operator, Param, Precedence, Statement,
};
use crate::error::ParseError;
use crate::lexer::{LexMode, Lexer};
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
                Keyword::Match => self.expr_stmt(),
                Keyword::Let => self.var_decl(),
                Keyword::Func => self.func_decl(),
                Keyword::Return => self.return_stmt(),
                Keyword::If => self.if_stmt(),
                // Else cannot be the start of a statement
                Keyword::Else => Err(ParseError::UnexpectedToken(self.next()?)),
            },
            // ExpressionStatement
            TokenKind::Ident(_) |
            // JSXExpression
            TokenKind::LessThan |
            // NumberExpression
            TokenKind::NumericLiteral(_) => self.expr_stmt(),
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

    fn next(&mut self) -> Result<Token> {
        let token = match self.lexer.next() {
            Ok(token) => Ok(token),
            Err(_) => Err(ParseError::LexError),
        };
        token
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

    fn jsx_expr(&mut self) -> Result<JSXChild> {
        self.lexer.mode = LexMode::Normal;
        let expr = self.expr(0)?;
        self.lexer.mode = LexMode::JSX;
        self.expect(TokenKind::RBrace)?;
        let child = JSXChild::JSXExpression(expr);
        Ok(child)
    }

    fn jsx_children(&mut self) -> Result<Option<Vec<JSXChild>>> {
        println!("start parsing jsx children");
        // Set self.lexer.mode = LexMode::MaybeJSXText ?
        let mut children: Vec<JSXChild> = vec![];
        loop {
            self.lexer.mode = LexMode::JSXText;
            let token = self.next()?;
            println!("jsx_children {:?}", token);
            // TODO make this peek so that jsx_close_element can expect
            // LessThan
            match token.kind {
                TokenKind::LessThan => {
                    // Move out of JSX mode, this might be the closing element.
                    self.lexer.mode = LexMode::Normal;
                    if self.peek()?.kind == TokenKind::Div {
                        println!("oh we done parsing children now!");
                        // Closing element, end of children
                        return Ok(Some(children));
                    }
                    println!("nested jsx element");
                    let element = self.jsx_element()?;
                    let child = JSXChild::JSXElement(Box::new(element));
                    children.push(child);
                }
                TokenKind::LBrace => {
                    let child = self.jsx_expr()?;
                    children.push(child);
                }
                TokenKind::JSXText(text) => {
                    let jsx_text = JSXText::new(text);
                    let child = JSXChild::JSXText(jsx_text);
                    children.push(child);
                }
                _ => {
                    println!("done parsing children");
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
        self.expect(TokenKind::Equals)?;
        let value = match self.peek()?.kind {
            TokenKind::StringLiteral(_) => {
                let string_token = self.next()?;
                Expression::StringLiteral(string_token)
            }
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                self.lexer.mode = LexMode::Normal;
                let expr = self.expr(0)?;
                self.lexer.mode = LexMode::JSX;
                self.expect(TokenKind::RBrace)?;
                expr
            }
            _ => return Err(ParseError::UnexpectedToken(self.next()?)),
        };
        Ok(JSXAttribute::new(name, Box::new(value)))
    }

    fn jsx_attributes(&mut self) -> Result<Option<Vec<JSXAttribute>>> {
        let mut attributes: Vec<JSXAttribute> = vec![];
        loop {
            match self.peek()?.kind {
                TokenKind::GreaterThan | TokenKind::Div => {
                    if attributes.is_empty() {
                        return Ok(None);
                    } else {
                        return Ok(Some(attributes));
                    }
                }
                TokenKind::Ident(_) => {
                    let attr = self.jsx_attribute()?;
                    attributes.push(attr);
                }
                _ => return Err(ParseError::UnexpectedToken(self.next()?)),
            }
        }
    }

    fn jsx_element(&mut self) -> Result<JSXElement> {
        let mode = self.lexer.mode;
        println!("jsx element, coming from mode {:?}", mode);
        self.lexer.mode = LexMode::JSX;
        let tag_name = self.ident()?;
        println!("jsx element {:?}", tag_name);
        let attrs = self.jsx_attributes()?;
        // TODO don't clone tag_name
        let open = JSXOpeningElement::new(tag_name.clone(), attrs);
        // Handle self closing elements
        let elem = if self.peek()?.kind == TokenKind::Div {
            self.expect(TokenKind::Div)?;
            self.expect(TokenKind::GreaterThan)?;
            JSXElement::new(open, None, None)
        } else {
            self.expect(TokenKind::GreaterThan)?;
            let children = self.jsx_children()?;
            let close = self.jsx_close_element()?;
            JSXElement::new(open, close, children)
        };
        self.lexer.mode = mode;
        Ok(elem)
    }

    fn match_arm(&mut self) -> Result<MatchArm> {
        let test = self.expr(0)?;
        self.expect(TokenKind::Arrow)?;
        let consequent = self.expr(0)?;
        self.expect(TokenKind::Comma)?;
        let res = Ok(MatchArm::new(test, consequent));
        println!("{:?}", res);
        res
    }

    fn match_expr(&mut self) -> Result<Expression> {
        let discriminant = self.expr(0)?;
        self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        loop {
            let case = self.match_arm()?;
            cases.push(case);
            if self.peek()?.kind == TokenKind::RBrace {
                break;
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Expression::MatchExpression {
            discriminant: Box::new(discriminant),
            cases,
        })
    }

    fn prefix_expr(&mut self) -> Result<Expression> {
        let token = self.next()?;
        match token.kind {
            // Literals
            TokenKind::NumericLiteral(_) => Ok(Expression::NumericLiteral(token)),
            TokenKind::StringLiteral(_) => Ok(Expression::StringLiteral(token)),
            TokenKind::Ident(name) => Ok(Expression::Identifier(Identifier(name))),
            // JSX Expresion
            TokenKind::LessThan => {
                println!("Mode before JSXEelement {:?}", self.lexer.mode);
                self.lexer.mode = LexMode::JSX;
                Ok(Expression::JSXExpression(self.jsx_element()?))
            }
            // Match Expression
            TokenKind::Keyword(Keyword::Match) => self.match_expr(),
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

    fn member_expr(&mut self, left: Expression) -> Result<Expression> {
        self.expect(TokenKind::Dot)?;
        let property = self.ident()?;
        Ok(Expression::MemberExpression {
            object: Box::new(left),
            property,
        })
    }

    fn call_expr(&mut self, ident: Expression) -> Result<Expression> {
        self.expect(TokenKind::LParen)?;
        println!("call expr {:?}", ident);
        // TODO handle multiple arguments
        let arg = self.expr(0)?;
        self.expect(TokenKind::RParen)?;
        Ok(Expression::CallExpression {
            callee: Box::new(ident),
            arguments: vec![arg],
        })
    }

    fn infix_expr(&mut self, left: Expression) -> Result<Expression> {
        println!("infix expr");
        match self.peek()?.kind {
            // ConditionalExpression
            TokenKind::Question => self.cond_expr(left),
            // CallExpression
            TokenKind::LParen => self.call_expr(left),
            // BinaryExpression
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Div
            | TokenKind::Mul
            | TokenKind::LessThan
            | TokenKind::DblEquals
            | TokenKind::GreaterThan => self.binary_expr(left),
            // MemberExpression
            TokenKind::Dot => self.member_expr(left),
            _ => Ok(left),
        }
    }

    fn peek_precedence(&mut self) -> Result<u32> {
        Ok(match self.peek()?.kind {
            TokenKind::LParen => Precedence::ASSIGNMENT as u32,
            TokenKind::Equals => Precedence::ASSIGNMENT as u32,
            TokenKind::Dot => Precedence::ASSIGNMENT as u32,
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
            println!("loop expr {:?}", expr);
            println!("percedence {}", self.peek_precedence()?);
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
