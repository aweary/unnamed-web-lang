use crate::parser::Parser;
use crate::result::Result;

use crate::ast::number_expr;
use crate::ast::{Call, ObjectProperty, Template, TemplateAttr};
use crate::ast::{Expr, ExprId, ExprKind, MatchArm, Precedence};
use crate::ast::{TemplateChild, TemplateId};
use crate::error::ParseError;
use crate::token::{Keyword, TokenKind};

use crate::lexer::LexMode;

macro_rules! optional_comma {
    ($self:ident, $kind:ident) => {
        // Commas are required between elements, with
        // the exception of trailing commas, which
        // are optional.
        if $self.eat(TokenKind::Comma)? {
            continue;
        } else {
            $self.expect(TokenKind::$kind)?;
            break;
        }
    };
}

pub trait ExprParser<'a> {
    fn peek_precedence(&mut self) -> Result<Precedence>;
    fn prefix_expr(&mut self) -> Result<ExprId>;
    fn cond_expr(&mut self, test: ExprId) -> Result<ExprId>;
    fn array_expr(&mut self) -> Result<ExprId>;
    fn obj_expr(&mut self) -> Result<ExprId>;
    fn infix_expr(&mut self, left: ExprId) -> Result<ExprId>;
    fn binary_expr(&mut self, left: ExprId) -> Result<ExprId>;
    fn logical_expr(&mut self, left: ExprId) -> Result<ExprId>;
    fn call_expr(&mut self, left: ExprId) -> Result<ExprId>;
    fn member_expr(&mut self, left: ExprId) -> Result<ExprId>;
    fn template(&mut self) -> Result<TemplateId>;
    fn template_expr(&mut self) -> Result<ExprId>;
    fn template_attr(&mut self) -> Result<TemplateAttr>;
    fn template_attrs(&mut self) -> Result<Vec<TemplateAttr>>;
    fn template_children(&mut self) -> Result<Option<Vec<TemplateChild>>>;
    fn match_arm_expr(&mut self) -> Result<MatchArm>;
    fn match_expr(&mut self) -> Result<ExprId>;
    fn expr_list(&mut self, terminator: TokenKind) -> Result<Vec<ExprId>>;
    fn expr(&mut self, precedence: Precedence) -> Result<ExprId>;
}

impl<'a> ExprParser<'a> for Parser<'a> {
    fn peek_precedence(&mut self) -> Result<Precedence> {
        let token = self.peek_token()?;
        Ok(token.precedence())
    }

    fn prefix_expr(&mut self) -> Result<ExprId> {
        let token = self.next_token()?;
        match token.kind {
            // Literals
            TokenKind::Number(num) => Ok(self.ctx.alloc_expr(number_expr(num))),
            TokenKind::String(sym) => Ok(self.ctx.alloc_expr(Expr::new(ExprKind::Str(sym)))),
            TokenKind::Ident(sym) => Ok(self.ctx.alloc_expr(Expr::new(ExprKind::Ident(sym)))),
            TokenKind::Bool(boolean) => Ok(self.ctx.alloc_expr(Expr::new(ExprKind::Bool(boolean)))),
            TokenKind::LBrace => self.array_expr(),
            TokenKind::LCurlyBrace => self.obj_expr(),
            // Unary
            TokenKind::Plus | TokenKind::Minus => {
                let expr_id = self.expr(Precedence::PREFIX)?;
                // This should never fail since we know this is
                // Plus | Minus and those both map to Op variants
                let op = token.to_op().unwrap();
                let id = self.ctx.alloc_expr(Expr::new(ExprKind::Unary(op, expr_id)));
                Ok(id)
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
            // JSX expression
            TokenKind::LessThan => self.template_expr(),
            // TODO JSXExpression
            _ => Err(ParseError::UnexpectedToken(token)),
        }
    }

    fn expr_list(&mut self, terminator: TokenKind) -> Result<Vec<ExprId>> {
        use TokenKind::Comma;
        let mut exprs = vec![];
        loop {
            if self.eat(terminator.clone())? {
                break;
            }
            let expr = self.expr(Precedence::NONE)?;
            exprs.push(expr);
            // If we find a comma, move on
            if self.eat(Comma)? {
                continue;
            } else {
                self.expect(terminator.clone())?;
                break;
            }
        }
        Ok(exprs)
    }

    fn template_attr(&mut self) -> Result<TemplateAttr> {
        use TokenKind::{Equals, LCurlyBrace, RCurlyBrace, String};
        let name = self.ident()?;
        self.expect(Equals)?;
        let value = match self.peek_token()?.kind {
            String(string) => {
                self.expect(String(string))?;
                self.ctx.alloc_expr(Expr::new(ExprKind::Str(string)))
            }
            LCurlyBrace => {
                self.expect(LCurlyBrace)?;
                self.lexer.mode = LexMode::Normal;
                let expr_id = self.expr(Precedence::NONE)?;
                self.lexer.mode = LexMode::JSX;
                self.expect(RCurlyBrace)?;
                expr_id
            }
            _ => return Err(ParseError::UnexpectedToken(self.next_token()?)),
        };
        Ok(TemplateAttr { name, value })
    }

    fn template_attrs(&mut self) -> Result<Vec<TemplateAttr>> {
        use TokenKind::{Div, GreaterThan, Ident};
        let mut attrs = vec![];
        loop {
            match self.peek_token()?.kind {
                GreaterThan | Div => return Ok(attrs),
                Ident(_) => {
                    let attr = self.template_attr()?;
                    attrs.push(attr);
                }
                _ => return Err(ParseError::UnexpectedToken(self.next_token()?)),
            }
        }
    }

    fn template_children(&mut self) -> Result<Option<Vec<TemplateChild>>> {
        use TokenKind::{Div, LCurlyBrace, LessThan, RCurlyBrace, TemplateText};
        let mut children = vec![];
        loop {
            self.lexer.mode = LexMode::TemplateText;
            let token = self.next_token()?;
            match token.kind {
                LessThan => {
                    // Move out of TemplateText mode, this might be a closing element
                    self.lexer.mode = LexMode::Normal;
                    if self.peek_token()?.kind == Div {
                        return Ok(Some(children));
                    }
                    let template_id = self.template()?;
                    let child = TemplateChild::Template(template_id);
                    children.push(child);
                }
                LCurlyBrace => {
                    self.lexer.mode = LexMode::Normal;
                    let expr_id = self.expr(Precedence::NONE)?;
                    self.lexer.mode = LexMode::JSX;
                    self.expect(RCurlyBrace)?;
                    let child = TemplateChild::Expr(expr_id);
                    children.push(child);
                }
                TemplateText(text) => {
                    let child = TemplateChild::Text(text);
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

    fn template(&mut self) -> Result<TemplateId> {
        use TokenKind::{Div, GreaterThan};
        self.lexer.mode = LexMode::JSX;
        let name = self.ident()?;
        let attrs = self.template_attrs()?;
        let template_id = if self.peek_token()?.kind == Div {
            // Self-closing, no children
            self.expect(Div)?;
            self.expect(GreaterThan)?;
            self.ctx.alloc_template(Template {
                name,
                attrs,
                children: None,
            })
        } else {
            self.expect(GreaterThan)?;
            let children = self.template_children()?;
            // template_children will eat the token for < here,
            // which is kind of weird but whatever...
            self.expect(Div)?;
            // Closing tag must match the opening tag.
            assert_eq!(name, self.ident()?);
            self.expect(GreaterThan)?;
            self.ctx.alloc_template(Template {
                name,
                attrs,
                children,
            })
        };
        Ok(template_id)
    }

    fn template_expr(&mut self) -> Result<ExprId> {
        let mode = self.lexer.mode;
        self.lexer.mode = LexMode::JSX;
        let template_id = self.template()?;
        self.lexer.mode = mode;
        let expr_id = self
            .ctx
            .alloc_expr(Expr::new(ExprKind::Template(template_id)));
        Ok(expr_id)
    }

    fn array_expr(&mut self) -> Result<ExprId> {
        let exprs = self.expr_list(TokenKind::RBrace)?;
        let expr_id = self.ctx.alloc_expr(Expr::new(ExprKind::Array(exprs)));
        Ok(expr_id)
    }

    fn obj_expr(&mut self) -> Result<ExprId> {
        use TokenKind::{Colon, Ident, RCurlyBrace};
        let mut properties = vec![];
        loop {
            match self.next_token()?.kind {
                RCurlyBrace => {
                    break;
                }
                Ident(sym) => {
                    self.expect(Colon)?;
                    let property = ObjectProperty(sym, self.expr(Precedence::NONE)?);
                    properties.push(property);
                    optional_comma!(self, RCurlyBrace);
                }
                _ => return Err(ParseError::UnexpectedToken(self.next_token().unwrap())),
            }
        }
        let expr_id = self.ctx.alloc_expr(Expr::new(ExprKind::Object(properties)));
        Ok(expr_id)
    }

    fn match_arm_expr(&mut self) -> Result<MatchArm> {
        let test = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Arrow)?;
        let consequent = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Comma)?;
        Ok(MatchArm { test, consequent })
    }

    fn match_expr(&mut self) -> Result<ExprId> {
        let discriminant = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::LCurlyBrace)?;
        let mut cases = vec![];
        loop {
            let case = self.match_arm_expr()?;
            cases.push(case);
            if self.peek_token()?.kind == TokenKind::RCurlyBrace {
                break;
            }
        }
        self.expect(TokenKind::RCurlyBrace)?;
        let expr_id = self.ctx.alloc_expr(Expr::new(ExprKind::Match {
            discriminant,
            cases,
        }));
        Ok(expr_id)
    }

    fn cond_expr(&mut self, test: ExprId) -> Result<ExprId> {
        self.expect(TokenKind::Question)?;
        let consequent = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Colon)?;
        let alternate = self.expr(Precedence::ASSIGNMENT)?;
        let expr_id = self.ctx.alloc_expr(Expr::new(ExprKind::Cond {
            test,
            consequent,
            alternate,
        }));
        Ok(expr_id)
    }

    fn call_expr(&mut self, callee: ExprId) -> Result<ExprId> {
        self.expect(TokenKind::LParen)?;
        let arguments = self.expr_list(TokenKind::RParen)?;
        let call = Call { callee, arguments };
        let expr_id = self.ctx.alloc_expr(Expr::new(ExprKind::Call(call)));
        Ok(expr_id)
    }

    fn member_expr(&mut self, obj: ExprId) -> Result<ExprId> {
        self.expect(TokenKind::Dot)?;
        let property = self.ident()?;
        let expr_id = self
            .ctx
            .alloc_expr(Expr::new(ExprKind::Member { obj, property }));
        Ok(expr_id)
    }

    fn infix_expr(&mut self, left: ExprId) -> Result<ExprId> {
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
            _ => Err(ParseError::UnexpectedEOF),
        }
    }

    fn binary_expr(&mut self, left: ExprId) -> Result<ExprId> {
        let (op, precedence) = {
            let token = self.next_token()?;
            // TODO or_else throw an unexpectedtoken error
            let op = token.to_op().unwrap();
            let precedence = token.precedence();
            (op, precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ExprKind::Binary { op, left, right };
        let expr_id = self.ctx.alloc_expr(Expr::new(kind));
        Ok(expr_id)
    }

    fn logical_expr(&mut self, left: ExprId) -> Result<ExprId> {
        let (op, precedence) = {
            let token = self.next_token()?;
            // TODO or_else throw an unexpectedtoken error
            let op = token.to_op().unwrap();
            let precedence = token.precedence();
            (op, precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ExprKind::Logical { op, left, right };
        let expr_id = self.ctx.alloc_expr(Expr::new(kind));
        Ok(expr_id)
    }

    fn expr(&mut self, precedence: Precedence) -> Result<ExprId> {
        let mut expr_id = self.prefix_expr()?;
        while precedence < self.peek_precedence()? {
            expr_id = self.infix_expr(expr_id)?;
        }
        Ok(expr_id)
    }
}
