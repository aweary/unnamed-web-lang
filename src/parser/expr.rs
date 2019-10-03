use super::result::Result;
use crate::parser::Parser;

use crate::ast::{Expr, ExprKind, Precedence, MatchArm};
use crate::error::ParseError;
use crate::token::{TokenKind, Keyword};


pub trait ExprParser<'a> {
    fn peek_precedence(&mut self) -> Result<Precedence>;
    fn prefix_expr(&mut self) -> Result<Expr>;
    fn cond_expr(&mut self, left: Expr) -> Result<Expr>;
    fn infix_expr(&mut self, left: Expr) -> Result<Expr>;
    fn binary_expr(&mut self, left: Expr) -> Result<Expr>;
    fn logical_expr(&mut self, left: Expr) -> Result<Expr>;
    fn call_expr(&mut self, left: Expr) -> Result<Expr>;
    fn member_expr(&mut self, left: Expr) -> Result<Expr>;
    fn match_arm_expr(&mut self) -> Result<MatchArm>;
    fn match_expr(&mut self) -> Result<Expr>;
    fn expr(&mut self, precedence: Precedence) -> Result<Expr>;
}

impl<'a> ExprParser<'a> for Parser<'a> {

    fn peek_precedence(&mut self) -> Result<Precedence> {
        let token = self.peek_token()?;
        Ok(token.precedence())
    }

    fn prefix_expr(&mut self) -> Result<Expr> {
        let token = self.next_token()?;
        match token.kind {
            // Literals
            TokenKind::Number(num) => Ok(Expr::new(ExprKind::Number(num))),
            TokenKind::String(sym) => Ok(Expr::new(ExprKind::Str(sym))),
            TokenKind::Ident(sym) => Ok(Expr::new(ExprKind::Ident(sym))),
            // Unary
            TokenKind::Plus | TokenKind::Minus => {
                let expr = self.expr(Precedence::PREFIX)?;
                // This should never fail since we know this is
                // Plus | Minus and those both map to Op variants
                let op = token.to_op().unwrap();
                Ok(Expr::new(ExprKind::Unary(op, Box::new(expr))))
            }
            // Group
            TokenKind::LParen => {
                // TODO should we have a variant for grouped expressions?
                // Might be important for tracking spans.
                let expr = self.expr(Precedence::NONE)?;
                self.expect(TokenKind::RParen)?;
                Ok(expr)
            }
            // Match
            TokenKind::Keyword(Keyword::Match) => self.match_expr(),
            // TODO JSXExpression
            _ => Err(ParseError::UnexpectedToken(token)),
        }
    }

    fn match_arm_expr(&mut self) -> Result<MatchArm> {
        let test = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Arrow)?;
        let consequent = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Comma)?;
        Ok(MatchArm {
            test: Box::new(test),
            consequent: Box::new(consequent),
        })
    }

    fn match_expr(&mut self) -> Result<Expr> {
        let discriminant = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::LBrace)?;
        let mut cases = vec![];
        loop {
            let case = self.match_arm_expr()?;
            cases.push(case);
            if self.peek_token()?.kind == TokenKind::RBrace {
                break;
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Expr::new(ExprKind::Match {
            discriminant: Box::new(discriminant),
            cases
        }))
    }

    fn cond_expr(&mut self, left: Expr) -> Result<Expr> {
        self.expect(TokenKind::Question)?;
        let test = Box::new(left);
        let consequent = Box::new(self.expr(Precedence::NONE)?);
        self.expect(TokenKind::Colon)?;
        let alternate = Box::new(self.expr(Precedence::ASSIGNMENT)?);
        Ok(Expr::new(ExprKind::Cond {
            test,
            consequent,
            alternate
        }))
    }

    fn call_expr(&mut self, left: Expr) -> Result<Expr> {
        self.expect(TokenKind::LParen)?;
        // TODO handle multiple arguments
        let arg = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::RParen)?;
        Ok(Expr::new(
            ExprKind::Call {
                callee: Box::new(left),
                arguments: vec![arg],
            }
        ))
    }

    fn member_expr(&mut self, left: Expr) -> Result<Expr> {
        self.expect(TokenKind::Dot)?;
        let property = self.ident()?;
        Ok(Expr::new(
            ExprKind::Member {
            obj: Box::new(left),
            property,
            }
        ))
    }

    fn infix_expr(&mut self, left: Expr) -> Result<Expr> {
        use TokenKind::*;
        match self.peek_token()?.kind {
            // Binary
            Plus | Minus | Div | Mul | LessThan | DblEquals | GreaterThan => self.binary_expr(left),
            // Conditional
            Question => self.cond_expr(left),
            // Logical
            And | Or => self.logical_expr(left),
            // Call
            LParen => self.call_expr(left),
            // Member
            Dot => self.member_expr(left),
            _ => Err(ParseError::UnexpectedEOF)
        }
    }

    fn binary_expr(&mut self, left: Expr) -> Result<Expr> {
        let (op, precedence) = {
            let token = self.next_token()?;
            // TODO or_else throw an unexpectedtoken error
            let op = token.to_op().unwrap();
            let precedence = token.precedence();
            (op,precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ExprKind::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
        Ok(Expr::new(kind))
    }

    fn logical_expr(&mut self, left: Expr) -> Result<Expr> {
        let (op, precedence) = {
            let token = self.next_token()?;
            // TODO or_else throw an unexpectedtoken error
            let op = token.to_op().unwrap();
            let precedence = token.precedence();
            (op, precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ExprKind::Logical {
            op,
            left: Box::new(left),
            right: Box::new(right),
        };
        Ok(Expr::new(kind))
    }

    fn expr(&mut self, precedence: Precedence) -> Result<Expr> {
        println!("Expr size: {}", std::mem::size_of::<Expr>());
        let mut expr = self.prefix_expr()?;
        while precedence < self.peek_precedence()? {
            expr = self.infix_expr(expr)?;
        }
        Ok(expr)
    }
}
