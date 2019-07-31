use crate::error;
use crate::lexer::Lexer;
use crate::token::{Keyword, Token, TokenKind};
use std::iter::Peekable;

type Result<T> = std::result::Result<T, error::Error>;

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
}

#[derive(Debug)]
struct Identifier(String);

#[derive(Debug)]
enum Precedence {
    GROUP = 0,
    ASSIGNMENT = 1,
    CONDITIONAL = 2,
    SUM = 3,
    PRODUCT = 4,
    EXPONENT = 5,
    PREFIX = 6,
    POSTFIX = 7,
    CALL = 8,
}

#[derive(Debug)]
enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Debug)]
enum Expression {
    NumericLiteral(Token),
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
}

#[derive(Debug)]
enum Statement {
    VariableDeclaration { id: Identifier, value: Expression },
    ExpressionStatement(Expression),
}

macro_rules! keyword {
    ($id:ident) => {
        TokenKind::Keyword(Keyword::$id)
    };
}

impl<'a> Parser<'a> {
    pub fn parse<'s>(source: &'s String) -> Parser<'s> {
        let lexer = Lexer::new(&source).peekable();
        let mut parser = Parser { lexer };
        // parse_module
        let stmts = parser.parse_statement_list();
        println!("{:#?}", stmts);
        parser
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.parse_expr(0)?;
        Ok(Statement::ExpressionStatement(expr))
    }

    fn parse_statement_list(&mut self) -> Result<Vec<Statement>> {
        let mut statements = vec![];
        loop {
            // Parse the statement...
            match self.lexer.peek() {
                Some(token) => {
                    match &token.kind {
                        // Some variant of ExpressionStatement
                        TokenKind::Ident(_)
                        | TokenKind::NumericLiteral(_)
                        | TokenKind::Plus
                        | TokenKind::Minus
                        | TokenKind::Star
                        | TokenKind::Slash
                        | TokenKind::LParen => {
                            statements.push(self.parse_expr_stmt()?);
                        }
                        // Declarations
                        TokenKind::Keyword(keyword) => {
                            match keyword {
                                // VariableDeclaration
                                Keyword::Let => statements.push(self.parse_variable_declaration()?),
                            }
                        }
                        _ => {
                            return Err(error::Error::UnexpectedToken(self.lexer.next().unwrap()));
                        }
                    }
                }
                None => return Ok(statements)
            }

            if let Some(token) = self.lexer.peek() {
                match token.kind {
                    TokenKind::Semi => {
                        self.expect(TokenKind::Semi)?;
                        continue
                    },
                    _ => {return Ok(statements)}
                }
                // ...
            } else {
                return Ok(statements)
            }
        }
        Ok(statements)
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        match self.lexer.next() {
            Some(token) => {
                if token.kind == kind {
                    Ok(token)
                } else {
                    Err(error::Error::UnexpectedToken(token))
                }
            }
            None => Err(error::Error::UnexpectedEOF),
        }
    }

    fn read_ident(&mut self) -> Result<Identifier> {
        // TODO how to deduplicate this w/ parse_expr?
        match self.lexer.next() {
            Some(token) => match token.kind {
                TokenKind::Ident(name) => {
                    let id = Identifier(name);
                    Ok(id)
                }
                _ => Err(error::Error::UnexpectedToken(token)),
            },
            None => Err(error::Error::UnexpectedEOF),
        }
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression> {
        match self.lexer.next() {
            Some(token) => match token.kind {
                // Literals
                TokenKind::NumericLiteral(_) => Ok(Expression::NumericLiteral(token)),
                TokenKind::Ident(name) => Ok(Expression::Identifier(Identifier(name))),
                // Unary operators
                TokenKind::Plus => {
                    let operand = self.parse_expr(Precedence::PREFIX as u32)?;
                    Ok(Expression::UnaryExpression {
                        operator: Operator::Plus,
                        expr: Box::new(operand),
                    })
                }
                // TODO dedupe
                TokenKind::Minus => {
                    let operand = self.parse_expr(Precedence::PREFIX as u32)?;
                    Ok(Expression::UnaryExpression {
                        operator: Operator::Minus,
                        expr: Box::new(operand),
                    })
                }
                // Group
                TokenKind::LParen => {
                    let expr = self.parse_expr(Precedence::GROUP as u32)?;
                    self.expect(TokenKind::RParen)?;
                    Ok(expr)
                }
                _ => Err(error::Error::UnexpectedToken(token)),
            },
            None => Err(error::Error::UnexpectedEOF),
        }
    }

    fn parse_cond_expr(&mut self, test: Expression) -> Result<Expression> {
        self.expect(TokenKind::Question)?;
        let test = Box::new(test);
        let consequent = Box::new(self.parse_expr(0)?);
        self.expect(TokenKind::Colon)?;
        let alternate = Box::new(self.parse_expr((Precedence::CONDITIONAL as u32) - 1)?);
        Ok(Expression::ConditionalExpression {
            test,
            consequent,
            alternate,
        })
    }

    fn parse_binary_expr(&mut self, left: Expression) -> Result<Expression> {
        // TODO move this out
        let (operator, precedence) = match self.lexer.next() {
            Some(token) => match token.kind {
                TokenKind::Plus => (Operator::Plus, Precedence::SUM as u32),
                TokenKind::Minus => (Operator::Minus, Precedence::SUM as u32),
                TokenKind::Star => (Operator::Mul, Precedence::PRODUCT as u32),
                TokenKind::Slash => (Operator::Div, Precedence::PRODUCT as u32),
                _ => return Err(error::Error::UnexpectedToken(token)),
            },
            None => return Err(error::Error::UnexpectedEOF),
        };
        // TODO handle other operators
        let right = self.parse_expr(precedence)?;
        Ok(Expression::BinaryExpression {
            operator,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression> {
        match self.lexer.peek() {
            Some(token) => match token.kind {
                // ConditionalExpression
                TokenKind::Question => self.parse_cond_expr(left),
                // BinaryExpression
                TokenKind::Plus | TokenKind::Minus | TokenKind::Slash | TokenKind::Star => {
                    self.parse_binary_expr(left)
                }
                _ => Ok(left),
            },
            None => Ok(left),
        }
    }

    fn peek_precedence(&mut self) -> u32 {
        match self.lexer.peek() {
            Some(token) => match token.kind {
                // TODO precedence isn't per-token, its per operation kind
                // so we should have a C-style precedence enum
                TokenKind::Equals => Precedence::ASSIGNMENT as u32,
                TokenKind::Question => Precedence::CONDITIONAL as u32,
                TokenKind::Plus => 3,
                TokenKind::Minus => 3,
                TokenKind::Star => 4,
                TokenKind::Slash => 4,
                _ => 0,
            },
            // TODO error?
            None => 0,
        }
    }

    fn parse_expr(&mut self, precedence: u32) -> Result<Expression> {
        // todo error handling
        let mut expr = self.parse_prefix_expr()?;
        while precedence < self.peek_precedence() {
            expr = self.parse_infix_expr(expr)?;
        }
        Ok(expr)
    }

    fn parse_variable_declaration(&mut self) -> Result<Statement> {
        // This should handle const or othher variable decalaration keywords
        // we might add later.
        self.expect(keyword!(Let))?;
        let ident = self.read_ident()?;
        self.expect(TokenKind::Equals)?;
        let expr = self.parse_expr(0)?;
        let statement = Statement::VariableDeclaration {
            id: ident,
            value: expr,
        };
        Ok(statement)
    }
}
