// renaming this Tokenizer for now because I'm tired of the word Lexer...
use lexer::LexMode;
use lexer::Lexer as Tokenizer;
use syntax::ast;
use syntax::precedence::Precedence;
use syntax::sess::ParseSess;
use syntax::token::{self, Keyword, Token, TokenKind};
use syntax::ty;
use syntax::Span;

use diagnostics::ParseResult as Result;

const DUMMY_NODE_ID: ast::NodeId = ast::NodeId(0);

pub struct Parser<'s> {
    tokenizer: Tokenizer<'s>,
    sess: &'s ParseSess,
    span: Span,
}

impl Parser<'_> {
    /// Returns a `Parser` instance using the `source` string
    /// as an input. This should be used sparingly as it assumes that
    /// the *entire source* will be contained within `source`, so imports
    /// wont work. It's mainly an internal debugging path.lexer
    ///
    /// ```
    /// # Example
    /// use parser::Parser;
    /// let source = String::from("let a = 10");
    /// let mut parser = Parser::new_from_str(&source);
    /// ```
    pub fn new_from_str<'a>(source: &'a str, sess: &'a ParseSess) -> Parser<'a> {
        let tokenizer = Tokenizer::new(source, sess);
        // Start off with an empty span
        let span = Span::new(0, 0);
        Parser {
            tokenizer,
            sess,
            span,
        }
    }

    /// Returns the next token from the tokenizer.
    fn next_token(&mut self) -> Result<Token> {
        let token = self.tokenizer.next_token()?;
        self.span = token.span;
        Ok(token)
    }

    fn eat(&mut self, kind: TokenKind) -> Result<bool> {
        if self.peek()?.kind == kind {
            self.expect(kind)?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn skip(&mut self) -> Result<()> {
        self.next_token()?;
        Ok(())
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token> {
        // TODO don't unwrap here.
        let token = self.next_token().unwrap();
        if token.kind != kind {
            Err(self.sess.fatal(
                "Unexpceted token",
                &format!("Expected {} but found {}", kind, token.kind),
                token.span,
            ))
        } else {
            Ok(token)
        }
    }

    fn peek(&mut self) -> Result<&Token> {
        self.tokenizer.peek_token()
    }

    fn peek_precedence(&mut self) -> Result<Precedence> {
        let token = self.peek()?;
        Ok(token.precedence())
    }

    fn ident(&mut self) -> Result<ast::Ident> {
        // TODO dont unwrap here, maybe next_token should use result
        let token = self.next_token().unwrap();
        match token.kind {
            TokenKind::Ident(symbol) => Ok(ast::Ident {
                name: symbol,
                span: token.span,
            }),
            _ => Err(self
                .sess
                .fatal("Unexpected token", "Expected an identifier", token.span)),
        }
    }

    /// Root call for the parser; returns the `ast::Program` node
    /// which contains all the AST nodes for all modules in the
    /// program.
    pub fn parse_program(&mut self) -> Result<ast::Program> {
        Ok(ast::Program {
            modules: vec![self.parse_module()?],
        })
    }

    /// Parse a single module, i.e., a single file. In the future
    /// we might support defining submodules within a single file.
    fn parse_module(&mut self) -> Result<ast::Mod> {
        let mut items: Vec<ast::Item> = vec![];
        while !self.peek()?.follows_item_list() {
            items.push(self.parse_item()?);
            self.eat(TokenKind::Semi)?;
        }
        Ok(ast::Mod { items })
    }

    /// A single item in a list of items
    fn parse_item(&mut self) -> Result<ast::Item> {
        match self.peek()?.kind {
            // Function definition
            TokenKind::Reserved(Keyword::Func) => self.parse_fn(),
            // Enum definition
            TokenKind::Reserved(Keyword::Enum) => self.parse_enum(),
            // Type definition
            TokenKind::Reserved(Keyword::Type) => self.parse_type(),
            // Everything else
            _ => {
                let token = self.next_token().unwrap();
                Err(self.sess.fatal(
                    "Unexpected token",
                    &format!(
                        "Expexcted `fn`, `component`, `enum`, or `type`, found {:?}",
                        token.kind
                    ),
                    token.span,
                ))
            }
        }
    }

    fn parse_type(&mut self) -> Result<ast::Item> {
        self.skip()?;
        let _name = self.ident()?;
        Err(self
            .sess
            .fatal("Unsupported", "Type definitions are unsupported", self.span))
    }

    fn parse_enum(&mut self) -> Result<ast::Item> {
        use TokenKind::{Comma, Ident, LCurlyBrace, RCurlyBrace, Reserved};
        self.expect(Reserved(Keyword::Enum))?;
        let lo = self.span;
        // TODO support polymorphic names, self.type_ident()
        let name = self.ident()?;
        let generics = self.generics()?;
        let mut variants = vec![];
        self.expect(LCurlyBrace)?;
        loop {
            match self.next_token()?.kind {
                Ident(symbol) => {
                    let ident = ast::Ident {
                        name: symbol,
                        span: self.span,
                    };
                    let variant = ast::Variant {
                        ident,
                        id: DUMMY_NODE_ID,
                        span: self.span,
                    };
                    variants.push(variant);
                    self.eat(Comma)?;
                }
                RCurlyBrace => {
                    break;
                }
                _ => {
                    return Err(self.sess.fatal(
                        "Enums are not yet supported",
                        "Unsupported",
                        self.span,
                    ))
                }
            }
        }
        let span = lo.merge(self.span);
        Ok(ast::Item {
            ident: name,
            id: DUMMY_NODE_ID,
            kind: ast::ItemKind::Enum(ast::EnumDef { variants }, generics),
            span,
        })
    }

    /// Parses a list of optional generics. Used for type annotations,
    /// as well as function, component, enum, and struct definitions.
    ///
    /// TODO: definition generics can be constrainted (foo<A: B>) but
    /// type annotations cannot be, need to differentiate that.
    fn generics(&mut self) -> Result<Option<ast::Generics>> {
        Ok(match self.peek()?.kind {
            // This function has a generic
            TokenKind::LessThan => {
                self.expect(TokenKind::LessThan)?;
                let mut params = vec![];
                loop {
                    match self.peek()?.kind {
                        TokenKind::Ident(_) => {
                            let ident = self.ident()?;
                            params.push(ident);
                        }
                        TokenKind::Comma => {
                            self.eat(TokenKind::Comma)?;
                            continue;
                        }
                        _ => break,
                    }
                }
                self.expect(TokenKind::GreaterThan)?;
                // ast::Generics
                Some(ast::Generics { params })
            }
            _ => None,
        })
    }

    fn ty(&mut self) -> Result<ast::Ty> {
        use ty::{LiteralTy, Ty};
        let ty_name = self.ident()?;
        let generics = self.generics()?;
        // TODO move this out somewhere. This maps tokens to the
        // built-in types that cannot be redefined.
        let ty = match ty_name.to_str() {
            // I haven't decided what kind of casing I want to use...
            // accept everything for now.
            "number" | "Number" => Ty::Literal(LiteralTy::Number),
            "string" | "String" => Ty::Literal(LiteralTy::String),
            "bool" | "Bool" | "boolean" | "Boolean" => Ty::Literal(LiteralTy::Bool),
            "Unit" => Ty::Unit,
            _ => Ty::Variable(ty_name, generics),
        };
        Ok(ty)
    }

    /// Parse a function definition
    fn parse_fn(&mut self) -> Result<ast::Item> {
        self.expect(TokenKind::Reserved(Keyword::Func))?;
        let lo = self.span;
        let name = self.ident()?;
        let generics = self.generics()?;
        let params = self.parse_fn_params()?;
        let return_ty = {
            if self.eat(TokenKind::Colon)? {
                Some(self.ty()?)
            } else {
                None
            }
        };
        let decl = ast::FnDecl {
            params,
            output: Box::new(return_ty),
        };
        // TODO actually parse this once it means something
        let header = ast::FnHeader { is_async: false };
        // TODO same here
        let body = self.block()?;
        let span = lo.merge(self.span);
        let kind = ast::ItemKind::Fn(Box::new(decl), header, generics, Box::new(body));
        Ok(ast::Item {
            // TODO
            id: DUMMY_NODE_ID,
            kind,
            span,
            ident: name,
        })
    }

    /// Parse a list of function parameters
    fn parse_fn_params(&mut self) -> Result<Vec<ast::Param>> {
        use TokenKind::{Comma, Ident, LBrace, LCurlyBrace, RParen};
        let mut params = vec![];
        self.expect(TokenKind::LParen)?;
        loop {
            match self.peek()?.kind {
                RParen => {
                    break;
                }
                Ident(_) | LCurlyBrace | LBrace => {
                    let param = self.parse_fn_param()?;
                    params.push(param);
                    if !self.eat(Comma)? {
                        break;
                    }
                }
                _ => {
                    self.next_token()?;
                    return Err(self.sess.fatal(
                        "Expected a funciton paramter",
                        "Found this shit",
                        self.span,
                    ));
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        Ok(params)
    }

    // fn type_ident(&mut )

    fn parse_fn_param(&mut self) -> Result<ast::Param> {
        let local = self.local_pattern()?;
        // TODO this is bad
        let lo = match local {
            ast::LocalPattern::Ident(_, span) => span,
            ast::LocalPattern::Object(_, span) => span,
            ast::LocalPattern::List(_, span) => span,
        };
        let ty = {
            match self.peek()?.kind {
                TokenKind::Colon => {
                    self.expect(TokenKind::Colon)?;
                    // There is a type annotation
                    Some(self.ty()?)
                }
                _ => None,
            }
        };
        let span = lo.merge(self.span);
        Ok(ast::Param {
            local,
            ty: Box::new(ty),
            id: DUMMY_NODE_ID,
            span,
        })
    }

    fn block(&mut self) -> Result<ast::Block> {
        self.expect(TokenKind::LCurlyBrace)?;
        let lo = self.span;
        let stmts = self.stmt_list()?;
        self.expect(TokenKind::RCurlyBrace)?;
        let span = lo.merge(self.span);
        Ok(ast::Block {
            span,
            stmts,
            id: DUMMY_NODE_ID,
        })
    }

    fn stmt_list(&mut self) -> Result<Vec<ast::Stmt>> {
        let mut stmts = vec![];
        // let mut terminated = false;
        while !self.peek()?.follows_item_list() {
            // if terminated {
            //     return Err(self.sess.fatal(
            //         "Expected a semicolon",
            //         "Only the last statement in a list may omit the semicolon",
            //         self.span,
            //     ));
            // }
            let mut stmt = self.stmt()?;
            if self.eat(TokenKind::Semi)? {
                // If there was a semicolon, extend the statement's
                // span to include it.
                stmt.span = stmt.span.merge(self.span);
                // Whether a statement has a semicolon is important
                // for implicit function return and evaluating block expressions
                stmt.has_semi = true;
            } else {
                // Only the last item in a statement list can omit the
                // semicolon. If this loop runs again, we need to throw
                // terminated = true;
            }
            stmts.push(stmt);
        }
        Ok(stmts)
    }

    fn stmt(&mut self) -> Result<ast::Stmt> {
        let token = self.peek().unwrap();
        match token.kind {
            TokenKind::Reserved(Keyword::Let) => {
                let local = self.local()?;
                let span = local.span;
                stmt(ast::StmtKind::Local(Box::new(local)), span)
            }
            // If statement
            TokenKind::Reserved(Keyword::If) => {
                let expr = self.if_expr()?;
                let span = expr.span;
                stmt(ast::StmtKind::Expr(Box::new(expr)), span)
            }
            // Return statement
            TokenKind::Reserved(Keyword::Return) => {
                self.expect(TokenKind::Reserved(Keyword::Return))?;
                let lo = self.span;
                let expr = self.expr(Precedence::NONE)?;
                let span = lo.merge(expr.span);
                stmt(ast::StmtKind::Return(Box::new(expr)), span)
            }
            // While statement
            TokenKind::Reserved(Keyword::While) => {
                self.expect(TokenKind::Reserved(Keyword::While))?;
                let lo = self.span;
                let condition = self.expr(Precedence::NONE)?;
                let block = self.block()?;
                let span = lo.merge(self.span);
                stmt(
                    ast::StmtKind::While(Box::new(condition), Box::new(block)),
                    span,
                )
            }
            // Try/catch statement
            TokenKind::Reserved(Keyword::Try) => {
                use TokenKind::{Ident, LCurlyBrace, LParen, RParen};
                self.expect(TokenKind::Reserved(Keyword::Try))?;
                let lo = self.span;
                let try_block = self.block()?;
                self.expect(TokenKind::Reserved(Keyword::Catch))?;
                let catch_param = match self.peek()?.kind {
                    LCurlyBrace => None,
                    LParen => {
                        self.expect(LParen)?;
                        let pattern = self.local_pattern()?;
                        self.expect(RParen)?;
                        Some(pattern)
                    }
                    Ident(_) => Some(self.local_pattern()?),
                    _ => {
                        self.skip()?;
                        return Err(self.sess.fatal("", "", self.span));
                    }
                };
                let catch_block = self.block()?;
                let span = lo.merge(self.span);
                self.sess.emit_warning("trycatch", "try", span);
                stmt(
                    ast::StmtKind::TryCatch(
                        Box::new(try_block),
                        catch_param,
                        Box::new(catch_block),
                    ),
                    span,
                )
                // ...
            }
            // Assume other tokens are meant to be parsed as expressions.
            // This might not be the best for error reporting, so we might
            // want to expand this match case out to be explicit
            _ => {
                let expr = self.expr(Precedence::NONE)?;
                let span = expr.span;
                stmt(ast::StmtKind::Expr(Box::new(expr)), span)
            } // _ => {
              //     self.skip()?;
              //     Err(self.sess.fatal("Unsupported statement", "here", self.span))
              // }
        }
    }

    fn array_pattern(&mut self) -> Result<(ast::LocalPattern)> {
        use TokenKind::{Comma, Ident, LBrace, RBrace};
        self.expect(LBrace)?;
        let lo = self.span;
        let mut items = vec![];
        // Array patterns do not allow for further destructurng, so all items must be identifiers
        loop {
            match self.peek()?.kind {
                Ident(_) => {
                    let ident = self.ident()?;
                    items.push(ident);
                }
                Comma => {
                    self.expect(Comma)?;
                    continue;
                }
                RBrace => {
                    break;
                }
                _ => {
                    self.skip()?;
                    return Err(self.sess.fatal("Unexpected token", "Unexpected", self.span));
                }
            }
        }
        self.expect(RBrace)?;
        let span = lo.merge(self.span);
        Ok(ast::LocalPattern::List(items, span))
    }

    fn obj_pattern(&mut self) -> Result<ast::LocalPattern> {
        use TokenKind::{Colon, Comma, RCurlyBrace};
        let mut patterns = vec![];
        self.expect(TokenKind::LCurlyBrace)?;
        let lo = self.span;
        loop {
            let key = self.ident()?;
            let lo = self.span;
            let value = match self.peek()?.kind {
                // Has a value, need to parse it
                Colon => {
                    self.expect(Colon)?;
                    self.local_pattern()?
                }
                // Valid following tokens, move on...
                Comma | RCurlyBrace => ast::LocalPattern::Ident(key.clone(), key.span),
                // Everything else is a syntax error
                _ => return Err(self.sess.fatal("Unexpected token", "Unexpected", self.span)),
            };
            let span = lo.merge(self.span);
            let property = ast::LocalObjectProperty { span, key, value };
            patterns.push(property);
            match self.peek()?.kind {
                // Next item
                Comma => {
                    self.expect(Comma)?;
                    continue;
                }
                // End of list, exit
                RCurlyBrace => break,
                // Everything else is a syntax error
                _ => return Err(self.sess.fatal("Unexpected token", "Unexpected", self.span)),
            }
        }
        self.expect(RCurlyBrace)?;
        let span = lo.merge(self.span);
        Ok(ast::LocalPattern::Object(patterns, span))
    }

    fn local_pattern(&mut self) -> Result<ast::LocalPattern> {
        use TokenKind::{Ident, LBrace, LCurlyBrace};
        match self.peek()?.kind {
            // Simple, single identifer
            Ident(_) => {
                let ident = self.ident()?;
                let span = ident.span;
                Ok(ast::LocalPattern::Ident(ident, span))
            }
            // Object destructure
            LCurlyBrace => self.obj_pattern(),
            // List destructure
            LBrace => self.array_pattern(),
            // ...
            _ => {
                self.skip()?;
                Err(self.sess.fatal("Unknown pattern", "unexpected", self.span))
            }
        }
    }

    fn local(&mut self) -> Result<ast::Local> {
        self.expect(TokenKind::Reserved(Keyword::Let))?;
        let lo = self.span;
        let pattern = self.local_pattern()?;
        self.expect(TokenKind::Equals)?;
        // TODO init is optional?
        let init = self.expr(Precedence::NONE)?;
        let span = lo.merge(self.span);
        Ok(ast::Local {
            id: DUMMY_NODE_ID,
            name: pattern,
            // TODO support explicit types
            ty: None,
            // Optional initializing expression.
            init: Some(Box::new(init)),
            span,
        })
    }

    fn expr(&mut self, precedence: Precedence) -> Result<ast::Expr> {
        let mut expr = self.prefix_expr()?;
        while precedence < self.peek_precedence()? {
            expr = self.infix_expr(expr)?;
        }
        Ok(expr)
    }

    // Expressions that begin with an identifier
    fn ident_expr(&mut self) -> Result<ast::Expr> {
        let ident = self.ident()?;
        let span = ident.span;
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            kind: ast::ExprKind::Reference(ident),
            span,
        })
    }

    fn assign_expr(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        use ast::ExprKind;
        // TODO assignment expressions should support other left-hand side
        // expressions such as indexed and memberexpressions.
        match left.kind {
            ExprKind::Reference(_) => {
                // Allowed...
            }
            _ => {
                return Err(self.sess.fatal(
                    "Invalid left hand assignment",
                    "Expected an identifier",
                    left.span,
                ))
            }
        }

        let lo = left.span;
        let (op, precedence) = {
            let token = self.next_token()?;
            let precedence = token.precedence();
            let op = token.to_assign_op().unwrap();
            (op, precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ast::ExprKind::Assign(op, Box::new(left), Box::new(right));
        let span = lo.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            kind,
            span,
        })
    }

    fn prefix_expr(&mut self) -> Result<ast::Expr> {
        match self.peek()?.kind {
            // Literal values such as numbers, strings, booleans
            TokenKind::Literal(_) => {
                let lit = self.parse_lit()?;
                let span = lit.span;
                Ok(ast::Expr {
                    id: DUMMY_NODE_ID,
                    kind: ast::ExprKind::Lit(lit),
                    span,
                })
            }
            // Array literals
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                let lo = self.span;
                let exprs = self.expr_list(TokenKind::RBrace)?;
                let span = lo.merge(self.span);
                Ok(ast::Expr {
                    id: DUMMY_NODE_ID,
                    kind: ast::ExprKind::Array(exprs),
                    span,
                })
            }
            // Identifier
            TokenKind::Ident(_) => self.ident_expr(),
            // Unary expressions
            TokenKind::Plus | TokenKind::Minus | TokenKind::Exclaim => {
                let token = self.next_token()?;
                let lo = self.span;
                let op = token.to_un_op().unwrap();
                let expr = self.expr(Precedence::PREFIX)?;
                let span = lo.merge(self.span);
                Ok(ast::Expr {
                    id: DUMMY_NODE_ID,
                    kind: ast::ExprKind::Unary(op, Box::new(expr)),
                    span,
                })
            }
            // Block expression OR object literal?
            TokenKind::LCurlyBrace => {
                let block = self.block()?;
                let span = block.span;
                Ok(ast::Expr {
                    id: DUMMY_NODE_ID,
                    kind: ast::ExprKind::Block(Box::new(block)),
                    span,
                })
            }
            // Group expression
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let lo = self.span;
                let mut expr = self.expr(Precedence::NONE)?;
                self.expect(TokenKind::RParen)?;
                // Update the expression to include the wrapping
                // parens
                let span = lo.merge(self.span);
                expr.span = span;
                Ok(expr)
            }
            // If expression
            TokenKind::Reserved(Keyword::If) => self.if_expr(),
            // For expression
            TokenKind::Reserved(Keyword::For) => self.for_expr(),
            // Tempalte expression
            TokenKind::LessThan => {
                let template = self.template()?;
                let span = template.span;
                Ok(ast::Expr {
                    id: DUMMY_NODE_ID,
                    span,
                    kind: ast::ExprKind::Template(template),
                })
            }
            // Match expression
            TokenKind::Reserved(Keyword::Match) => self.match_expr(),
            _ => {
                self.skip()?;
                Err(self.sess.fatal(
                    "Failed to parse an expression",
                    "We expected an expression here",
                    self.span,
                ))
            }
        }
    }

    fn template(&mut self) -> Result<ast::Template> {
        use TokenKind::LessThan;
        self.expect(LessThan)?;
        let lo = self.span;
        self.tokenizer.set_mode(LexMode::JSX);
        self.finish_template(lo)
    }

    fn finish_template(&mut self, lo: Span) -> Result<ast::Template> {
        use TokenKind::{Div, GreaterThan};
        let name = self.ident()?;
        let attrs = self.template_attrs()?;
        let open = ast::TemplateOpenTag {
            name: name.clone(),
            attrs,
            span: lo.merge(self.span),
        };
        let template = if self.peek()?.kind == Div {
            self.expect(Div)?;
            self.expect(GreaterThan)?;
            let span = open.span.merge(self.span);
            ast::Template {
                id: DUMMY_NODE_ID,
                open,
                close: None,
                children: None,
                span,
            }
        } else {
            self.expect(GreaterThan)?;
            let children = self.template_children()?;
            let close = {
                let lo = self.span;
                // template_children will eat the token for < here,
                // which is kind of weird but whatever...
                self.expect(Div)?;
                let close_name = self.ident()?;
                self.expect(GreaterThan)?;
                let span = lo.merge(self.span);
                ast::TemplateCloseTag {
                    name: close_name,
                    span,
                }
            };
            let span = lo.merge(self.span);
            ast::Template {
                id: DUMMY_NODE_ID,
                open,
                close: Some(close),
                children,
                span,
            }
        };
        Ok(template)
    }

    fn template_attrs(&mut self) -> Result<Vec<ast::TemplateAttr>> {
        use TokenKind::{Div, GreaterThan, Ident};
        let mut attrs = vec![];
        loop {
            match self.peek()?.kind {
                GreaterThan | Div => return Ok(attrs),
                Ident(_) => {
                    let attr = self.template_attr()?;
                    attrs.push(attr);
                }
                _ => {
                    self.skip()?;
                    return Err(self.sess.fatal(
                        "Unexpected token",
                        "Expected <, /, or an identifier",
                        self.span,
                    ));
                }
            }
        }
    }

    fn template_attr(&mut self) -> Result<ast::TemplateAttr> {
        use TokenKind::{Equals, LCurlyBrace, Literal, RCurlyBrace};
        let name = self.ident()?;
        let lo = name.span;
        self.expect(Equals)?;
        let value = match self.peek()?.kind {
            // We support parsing strings, numbers, and booleans without
            // wrapping curly braces.
            Literal(_) => {
                let lit = self.parse_lit()?;
                let span = lit.span;

                ast::Expr {
                    id: DUMMY_NODE_ID,
                    span,
                    kind: ast::ExprKind::Lit(lit),
                }
            }
            LCurlyBrace => {
                self.expect(LCurlyBrace)?;
                self.tokenizer.set_mode(LexMode::Normal);
                let expr = self.expr(Precedence::NONE)?;
                self.tokenizer.set_mode(LexMode::JSX);
                self.expect(RCurlyBrace)?;
                expr
            }
            _ => {
                self.skip()?;
                return Err(self.sess.fatal(
                    "Unexpected token",
                    "trying to parse template attributes",
                    self.span,
                ));
            }
        };
        let span = lo.merge(self.span);
        Ok(ast::TemplateAttr { name, value, span })
    }

    fn template_children(&mut self) -> Result<Option<Vec<ast::TemplateChild>>> {
        use TokenKind::{Div, LCurlyBrace, LessThan, RCurlyBrace, TemplateText};
        let mut children = vec![];
        loop {
            self.tokenizer.set_mode(LexMode::TemplateText);
            match &self.peek()?.kind {
                LessThan => {
                    self.expect(LessThan)?;
                    let lo = self.span;
                    // Move out of TemplateText mode, this might be a closing element
                    self.tokenizer.set_mode(LexMode::Normal);
                    if self.peek()?.kind == Div {
                        return Ok(Some(children));
                    }
                    let template = self.finish_template(lo)?;
                    let child = ast::TemplateChild::Template(Box::new(template));
                    children.push(child);
                }
                LCurlyBrace => {
                    self.expect(LCurlyBrace)?;
                    let lo = self.span;
                    self.tokenizer.set_mode(LexMode::Normal);
                    let mut expr = self.expr(Precedence::NONE)?;
                    self.tokenizer.set_mode(LexMode::JSX);
                    self.expect(RCurlyBrace)?;
                    let span = lo.merge(self.span);
                    // Include the wrapping curly braces in the span for this expression
                    expr.span = span;
                    let child = ast::TemplateChild::Expr(Box::new(expr));
                    children.push(child);
                }
                TemplateText(text) => {
                    let text = text.clone();
                    // TODO self.expect_template_text()? maybe
                    self.skip()?;
                    let child = ast::TemplateChild::Text(text);
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

    fn for_expr(&mut self) -> Result<ast::Expr> {
        self.expect(TokenKind::Reserved(Keyword::For))?;
        let lo = self.span;
        let pattern = self.local_pattern()?;
        self.expect(TokenKind::Reserved(Keyword::In))?;
        let expr = self.expr(Precedence::NONE)?;
        let block = self.block()?;
        let span = lo.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            span,
            kind: ast::ExprKind::For(pattern, Box::new(expr), Box::new(block)),
        })
    }

    fn if_expr(&mut self) -> Result<ast::Expr> {
        self.expect(TokenKind::Reserved(Keyword::If))?;
        let lo = self.span;
        let condition = self.expr(Precedence::NONE)?;
        let consequent = self.block()?;
        // TODO, support `if else`
        let alt = if self.eat(TokenKind::Reserved(Keyword::Else))? {
            let block = self.block()?;
            Some(Box::new(block))
        } else {
            None
        };
        let span = lo.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            kind: ast::ExprKind::If(Box::new(condition), Box::new(consequent), alt),
            span,
        })
    }

    fn expect_lit(&mut self) -> Result<token::Lit> {
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Literal(lit) => Ok(lit),
            _ => Err(self
                .sess
                .fatal("Expected literal", "found this", token.span)),
        }
    }

    fn parse_lit(&mut self) -> Result<ast::Lit> {
        match self.peek()?.kind {
            // Literals such as numbers, strings, and booleans
            TokenKind::Literal(_) => {
                let token::Lit { kind, symbol } = self.expect_lit()?;
                use token::LitKind::*;
                let kind = match kind {
                    Number => ast::LitKind::Number(symbol),
                    Bool => ast::LitKind::Bool(symbol),
                    Str => ast::LitKind::Str(symbol),
                };
                Ok(ast::Lit {
                    span: self.span,
                    kind,
                })
            }

            _ => Err(self.sess.fatal("expected literal", "lol", self.span)),
        }
    }

    fn infix_expr(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        use TokenKind::*;
        match self.peek()?.kind {
            // Binary
            Plus | Minus | Div | Mul | LessThan | GreaterThan | DblEquals | And | Or | Pipeline => {
                self.binary_expr(left)
            }
            // Assignment
            Equals | PlusEquals => self.assign_expr(left),
            // Conditional
            Question => self.cond_expr(left),
            // Call
            LParen => self.call_expr(left),
            // Member
            Dot => self.member_expr(left),
            QuestionDot => self.optional_member_expr(left),
            _ => {
                self.skip()?;
                Err(self
                    .sess
                    .fatal("Unknown infix expression", "Here", self.span))
            }
        }
    }

    fn match_expr(&mut self) -> Result<ast::Expr> {
        self.expect(TokenKind::Reserved(Keyword::Match))?;
        let lo = self.span;
        let cond = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::LCurlyBrace)?;
        let mut cases = vec![];
        loop {
            let case = self.match_arm_expr()?;
            cases.push(case);
            if self.peek()?.kind == TokenKind::RCurlyBrace {
                break;
            }
        }
        self.expect(TokenKind::RCurlyBrace)?;
        let kind = ast::ExprKind::Match(Box::new(cond), cases);
        let span = lo.merge(self.span);
        let expr = ast::Expr {
            id: DUMMY_NODE_ID,
            span,
            kind,
        };
        Ok(expr)
    }

    fn match_arm_expr(&mut self) -> Result<ast::MatchArm> {
        use ast::ExprKind;
        let test = self.expr(Precedence::NONE)?;
        // Match arm test expression are restricted.
        match test.kind {
            ExprKind::Lit(_) | ExprKind::Member(..) | ExprKind::Reference(_) => {
                // All allowed
            }
            _ => {
                return Err(self.sess.fatal(
                    "Unsupported expression in match arm",
                    "Match arm expressions are limited to references",
                    test.span,
                ))
            }
        }
        self.expect(TokenKind::Arrow)?;
        // TODO
        let _consequent = self.expr(Precedence::NONE)?;
        let _span = test.span.merge(self.span);
        self.eat(TokenKind::Comma)?;
        Ok(ast::MatchArm {})
    }

    fn member_expr(&mut self, obj: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::Dot)?;
        let property = self.ident()?;
        let span = obj.span.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            span,
            kind: ast::ExprKind::Member(Box::new(obj), property),
        })
    }

    fn optional_member_expr(&mut self, obj: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::QuestionDot)?;
        let property = self.ident()?;
        let span = obj.span.merge(self.span);
        self.sess.emit_warning("", "", span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            span,
            kind: ast::ExprKind::Member(Box::new(obj), property),
        })
    }

    fn expr_list(&mut self, terminator: TokenKind) -> Result<Vec<ast::Expr>> {
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

    fn call_expr(&mut self, callee: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::LParen)?;
        let lo = callee.span;
        let args = self.expr_list(TokenKind::RParen)?;
        let span = lo.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            span,
            kind: ast::ExprKind::Call(Box::new(callee), args),
        })
    }

    fn binary_expr(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        let lo = left.span;
        let (op, precedence) = {
            let token = self.next_token()?;
            let precedence = token.precedence();
            let op = token.to_bin_op().unwrap();
            (op, precedence)
        };
        let right = self.expr(precedence)?;
        let kind = ast::ExprKind::Binary(op, Box::new(left), Box::new(right));
        let span = lo.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            kind,
            span,
        })
    }

    fn cond_expr(&mut self, test: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::Question)?;
        let consequent = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Colon)?;
        let alt = self.expr(Precedence::ASSIGNMENT)?;
        let span = test.span.merge(self.span);
        Ok(ast::Expr {
            id: DUMMY_NODE_ID,
            kind: ast::ExprKind::Cond(Box::new(test), Box::new(consequent), Box::new(alt)),
            span,
        })
    }
}

#[inline]
fn stmt(kind: ast::StmtKind, span: Span) -> Result<ast::Stmt> {
    Ok(ast::Stmt {
        has_semi: false,
        id: DUMMY_NODE_ID,
        kind,
        span,
    })
}

impl Iterator for Parser<'_> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(token) => match token.kind {
                TokenKind::EOF => None,
                _ => Some(token),
            },
            Err(_) => None,
        }
    }
}
