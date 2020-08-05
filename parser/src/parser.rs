// renaming this Tokenizer for now because I'm tired of the word Lexer...
use lexer::LexMode;
use lexer::Lexer as Tokenizer;
use syntax::ast;
use syntax::precedence::Precedence;
use syntax::token::{self, Keyword, Token, TokenKind};

use std::path::PathBuf;

use diagnostics::ParseResult as Result;
// use diagnostics::{Diagnostic, Label};

use source::diagnostics::{Diagnostic, Label, Span};

use log::debug;

const DUMMY_NODE_ID: ast::NodeId = ast::NodeId(0);

pub struct Parser<'s> {
    /// The tokenizer/lexer for this Parser instance
    tokenizer: Tokenizer<'s>,
    /// The span of the current token
    span: Span,
    /// Whether parsing a trailing closure is allowed
    allow_trailing_closure: bool,
    /// Whether newline characters are semantically meaningful
    is_newline_significant: bool,
    /// The character we know delimits expressions
    expr_terminator: Option<TokenKind>,
}

/// TODO move to diagnostics crate
trait DiagnosticReporting {
    fn fatal(&self, message: &str, label: &str, span: Span) -> Diagnostic;
}

impl DiagnosticReporting for Parser<'_> {
    fn fatal(
        &self,
        message: &str,
        label_message: &str,
        span: Span,
    ) -> Diagnostic {
        let label = Label::primary(span).with_message(label_message);
        Diagnostic::error()
            .with_message(message)
            .with_labels(vec![label])
    }
}

impl Parser<'_> {
    pub fn new(source: &str) -> Parser<'_> {
        debug!("Parser::new");
        // Create a tokenzier/lexer
        let tokenizer = Tokenizer::new(&source);
        // Start with a dummy span
        let span = Span::new(0, 0);
        Parser {
            tokenizer,
            span,
            allow_trailing_closure: true,
            is_newline_significant: false,
            expr_terminator: None,
        }
    }

    /// Returns the next token from the tokenizer.
    fn next_token(&mut self) -> Result<Token> {
        let token = self.tokenizer.next_token()?;
        if token.kind == TokenKind::Newline && !self.is_newline_significant {
            // Skip insignificant newlines
            self.next_token()
        } else {
            debug!("next_token: {:?}", token.kind);
            self.span = token.span;
            Ok(token)
        }
    }

    fn peek(&mut self) -> Result<&Token> {
        let peek_kind = &self.tokenizer.peek_token()?.kind;
        debug!(
            "peek_token: {:#?}, {}",
            peek_kind, self.is_newline_significant
        );
        if peek_kind == &TokenKind::Newline && !self.is_newline_significant {
            self.tokenizer.next_token()?;
            self.peek()
        } else {
            self.tokenizer.peek_token()
        }
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
        debug!("expect {:?}", kind);
        let prev_span = self.span;
        let token = self.next_token()?;
        if token.kind != kind {
            let diagnostic = Diagnostic::error()
                .with_message("Unexpected token")
                .with_labels(vec![
                    Label::primary(prev_span).with_message(&format!(
                        "Expected {} after this token",
                        kind
                    )),
                    Label::secondary(self.span)
                        .with_message("But we found this insteasd"),
                ]);
            Err(diagnostic)
        } else {
            Ok(token)
        }
    }

    fn peek_precedence(&mut self) -> Result<Precedence> {
        let allow_trailing_closure = self.allow_trailing_closure;
        debug!("allow_trailing_closure: {}", allow_trailing_closure);
        self.is_newline_significant = true;
        let token = self.peek()?;
        debug!("peek_precedence of {:#?}", token);
        let precedence = match token.kind {
            // Check if trailing closures are allowed
            TokenKind::LCurlyBrace => {
                if allow_trailing_closure {
                    Ok(token.precedence())
                } else {
                    Ok(Precedence::NONE)
                }
            }
            _ => Ok(token.precedence()),
        }?;
        self.is_newline_significant = false;
        Ok(precedence)
    }

    fn ident(&mut self) -> Result<ast::Ident> {
        // TODO dont unwrap here, maybe next_token should use result
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Ident(symbol) => Ok(ast::Ident {
                symbol,
                span: token.span,
            }),
            _ => Err(self.fatal(
                "Unexpected token",
                "Expected an identifier",
                token.span,
            )),
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
    pub fn parse_module(&mut self) -> Result<ast::Module> {
        let mut items: Vec<ast::Item> = vec![];
        while !self.peek()?.follows_item_list() {
            items.push(self.parse_item()?);
            self.eat(TokenKind::Semi)?;
        }
        Ok(ast::Module { items })
    }

    /// A single item in a list of items
    fn parse_item(&mut self) -> Result<ast::Item> {
        let token = self.peek()?;
        debug!("parse_item: {:#?}", token.kind);
        match token.kind {
            // Function definition
            TokenKind::Reserved(Keyword::Func) => self.parse_fn(false),
            TokenKind::Reserved(Keyword::Async) => self.async_def(),
            // Component definition
            TokenKind::Reserved(Keyword::Component) => {
                self.parse_component(false)
            }
            // Enum definition
            TokenKind::Reserved(Keyword::Enum) => self.parse_enum(),
            // Type definition
            TokenKind::Reserved(Keyword::Type) => self.parse_type_alias(),
            // Import declaration
            TokenKind::Reserved(Keyword::Import) => self.import(),
            // Constant declaration
            TokenKind::Reserved(Keyword::Const) => self.constant(),
            // Struct definition
            TokenKind::Reserved(Keyword::Struct) => self.parse_struct(),
            // Item declaration, but public
            TokenKind::Reserved(Keyword::Pub) => {
                self.expect(TokenKind::Reserved(Keyword::Pub))?;
                let lo = self.span;
                let item = self.parse_item()?;
                Ok(ast::Item {
                    span: lo.merge(item.span),
                    kind: ast::ItemKind::Export(Box::new(item)),
                })
            }

            // Everything else
            _ => {
                let token = self.next_token()?;
                Err(self.fatal(
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

    fn constant(&mut self) -> Result<ast::Item> {
        self.expect(TokenKind::Reserved(Keyword::Const))?;
        let lo = self.span;
        let name = self.ident()?;
        let ty = if self.eat(TokenKind::Colon)? {
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(TokenKind::Equals)?;
        let value = self.expr(Precedence::NONE)?;
        let span = lo.merge(self.span);
        let constant = ast::Constant {
            name,
            ty,
            value,
            span,
        };
        Ok(ast::Item {
            span,
            kind: ast::ItemKind::Constant(constant),
        })
    }

    fn import(&mut self) -> Result<ast::Item> {
        use Keyword::{Import, ImportFrom};
        use TokenKind::Reserved;
        self.expect(Reserved(Import))?;
        let lo = self.span;
        let specifiers = self.import_specifiers()?;
        self.expect(Reserved(ImportFrom))?;
        let path = self.import_path()?;
        let span = lo.merge(path.span);
        Ok(ast::Item {
            span,
            kind: ast::ItemKind::Import(Box::new(ast::Import {
                specifiers,
                span,
                path,
            })),
        })
    }

    fn import_specifiers(&mut self) -> Result<Vec<ast::ImportSpecifier>> {
        let mut specifiers = vec![];
        // We don't support default imports/exports, so always expect braces
        self.expect(TokenKind::LCurlyBrace)?;
        loop {
            match self.peek()?.kind {
                // Specifier
                TokenKind::Ident(_) => {
                    let ident = self.ident()?;
                    let lo = self.span;
                    let mut alias = None;
                    // If the next token is `as`, we have an alias
                    if let TokenKind::Reserved(Keyword::As) = self.peek()?.kind
                    {
                        self.expect(TokenKind::Reserved(Keyword::As))?;
                        alias = Some(self.ident()?);
                    }

                    let specifier = ast::ImportSpecifier {
                        span: lo.merge(self.span),
                        ident,
                        alias,
                    };
                    specifiers.push(specifier);
                    // Expect either a comma (meaning there's another specifier) or
                    // a right curly brace (meaning the list has ended)
                    match self.peek()?.kind {
                        // Either are fine, skip
                        TokenKind::Comma => {
                            self.eat(TokenKind::Comma)?;
                        }
                        TokenKind::RCurlyBrace => {
                            break;
                        }
                        // Fatal
                        _ => {
                            let token = self.next_token()?;
                            return Err(self.fatal(
                                "Unexpected token",
                                &format!(
                                    "Expexcted a comma, found {:?}",
                                    token.kind
                                ),
                                token.span,
                            ));
                        }
                    }
                }
                // End of specifiers, break out
                TokenKind::RCurlyBrace => {
                    break;
                }
                // All other tokens are syntax errors
                _ => {
                    let token = self.next_token()?;
                    return Err(self.fatal(
                        "Unexpected token",
                        &format!(
                            "Expexcted an identifier, found {:?}",
                            token.kind
                        ),
                        token.span,
                    ));
                }
            }
        }
        // ...
        self.expect(TokenKind::RCurlyBrace)?;
        Ok(specifiers)
    }

    fn import_path(&mut self) -> Result<ast::ImportPath> {
        if let TokenKind::Literal(lit) = self.next_token()?.kind {
            if lit.kind == token::LitKind::Str {
                let mut path_str = lit.symbol.as_str();
                // Strip the quotes from the symbol
                path_str = &path_str[1..path_str.len() - 1];
                let path = PathBuf::from(path_str);
                return Ok(ast::ImportPath {
                    path,
                    span: self.span,
                });
            }
        }
        Err(self.fatal("Expected import path", "found this", self.span))
    }

    fn parse_type_alias(&mut self) -> Result<ast::Item> {
        self.expect(TokenKind::Reserved(Keyword::Type))?;
        let name = self.ident()?;
        let lo = self.span;

        let parameters = if self.eat(TokenKind::LessThan)? {
            let mut parameters = vec![];
            loop {
                match self.peek()?.kind {
                    TokenKind::Ident(_) => {
                        let name = self.ident()?;
                        parameters.push(name)
                    }
                    TokenKind::Comma => {
                        self.expect(TokenKind::Comma)?;
                        continue;
                    }
                    _ => break,
                }
            }
            self.expect(TokenKind::GreaterThan)?;
            Some(parameters)
        // Type definition has generics
        } else {
            None
        };

        self.expect(TokenKind::Equals)?;
        let ty = self.parse_type()?;
        let span = lo.merge(self.span);
        let typedef = ast::TypeAlias {
            name,
            ty,
            parameters,
            span,
        };
        Ok(ast::Item {
            kind: ast::ItemKind::Type(typedef),
            span,
        })
        // Err(Diagnostic::error().with_message("Cant parse type definition"))
    }

    fn parse_prefix_type(&mut self) -> Result<ast::Type> {
        let lo = self.span;
        match self.peek()?.kind {
            // // Record types
            TokenKind::LCurlyBrace => Err(Diagnostic::error()
                .with_message("We dont support record types yet")
                .with_labels(vec![Label::primary(lo)])),
            // Tuple type
            TokenKind::LParen => {
                self.expect(TokenKind::LParen)?;
                let mut types = vec![];
                loop {
                    match self.peek()?.kind {
                        TokenKind::Ident(_) | TokenKind::LParen => {
                            let ty = self.parse_type()?;
                            types.push(ty);
                        }
                        _ => break,
                    }
                    self.eat(TokenKind::Comma)?;
                }
                self.expect(TokenKind::RParen)?;
                let span = lo.merge(self.span);
                Ok(ast::Type::Tuple(types, span))
            }
            TokenKind::Ident(_) => {
                let name = self.ident()?;
                let lo = self.span;

                match name.symbol.as_str() {
                    "number" => Ok(ast::Type::Number(name.span)),
                    "bool" => Ok(ast::Type::Boolean(name.span)),
                    "string" => Ok(ast::Type::String(name.span)),
                    "unit" => Ok(ast::Type::Unit(name.span)),
                    _ => {
                        let arguments = if self.eat(TokenKind::LessThan)? {
                            let mut arguments = vec![];
                            loop {
                                match self.peek()?.kind {
                                    TokenKind::Ident(_) => {
                                        let ty = self.parse_type()?;
                                        arguments.push(ty);
                                    }
                                    TokenKind::Comma => {
                                        self.eat(TokenKind::Comma)?;
                                    }
                                    _ => break,
                                }
                            }
                            self.expect(TokenKind::GreaterThan)?;
                            Some(arguments)
                        } else {
                            None
                        };
                        let span = lo.merge(self.span);

                        debug!("reference type {:?} : {:?}", name, arguments);
                        Ok(ast::Type::Reference {
                            name,
                            arguments,
                            span,
                        })
                    }
                }
            }
            _ => {
                let token = self.next_token()?;
                return Err(self.fatal(
                    "Unexpected token",
                    "Not a valid character for a type",
                    token.span,
                ));
            }
        }
    }

    fn parse_type(&mut self) -> Result<ast::Type> {
        debug!("parse_type");

        let ty = self.parse_prefix_type()?;
        debug!("prefix_type {:#?}", ty);
        match self.peek()?.kind {
            TokenKind::Arrow => {
                self.expect(TokenKind::Arrow)?;
                let out_ty = self.parse_type()?;
                Ok(ast::Type::Function(ty.into(), out_ty.into()))
            }
            TokenKind::LBrace => {
                let lo = ty.span();
                self.expect(TokenKind::LBrace)?;
                self.expect(TokenKind::RBrace)?;
                let span = lo.merge(self.span);
                Ok(ast::Type::List(ty.into(), span))
            }
            _ => Ok(ty),
        }
    }

    fn parse_enum(&mut self) -> Result<ast::Item> {
        use TokenKind::{
            Comma, Equals, Ident, LCurlyBrace, LParen, RCurlyBrace, RParen,
            Reserved,
        };
        self.expect(Reserved(Keyword::Enum))?;
        let lo = self.span;
        // TODO support polymorphic names, self.type_ident()
        let name = self.ident()?;
        // let generics = self.generics()?;
        let parameters = self.type_parameter_list()?;
        let mut variants = vec![];
        self.expect(LCurlyBrace)?;
        // let mut has_discriminant = false;
        loop {
            match self.peek()?.kind {
                Ident(_) => {
                    let ident = self.ident()?;
                    let lo = self.span;
                    // Enum variants allow initializers to provide runtime values
                    let discriminant = if self.eat(Equals)? {
                        // Track that this enum uses a discriminant in at least one of the
                        // variants.
                        // has_discriminant = true;
                        // TODO this should not be an arbitrary expression. Should
                        // be a literal only.
                        Some(self.expr(Precedence::NONE)?)
                    } else {
                        None
                    };

                    let fields = if self.eat(LParen)? {
                        let mut fields = vec![];
                        loop {
                            match self.peek()?.kind {
                                Ident(_) => {
                                    let ty = self.parse_type()?;
                                    fields.push(ty);
                                }
                                Comma => {
                                    self.eat(Comma)?;
                                }
                                _ => break,
                            }
                        }
                        self.expect(RParen)?;
                        // A tuple variant
                        Some(fields)
                    } else {
                        None
                    };

                    let variant = ast::Variant {
                        ident,
                        discriminant,
                        fields,
                        span: lo.merge(self.span),
                    };
                    variants.push(variant);
                    // Commas are not optional, even trailing ones.
                    // TODO consider supporting optional trailing commas
                    // self.expect(Comma)?;
                }
                RCurlyBrace => {
                    break;
                }
                // TODO macro for this repetition
                _ => {
                    self.next_token()?;
                    return Err(self.fatal(
                        "Unexpected token when parsing enum",
                        "Expected an identifier or }",
                        self.span,
                    ));
                }
            }
        }
        self.expect(RCurlyBrace)?;
        let span = lo.merge(self.span);
        Ok(ast::Item {
            kind: ast::ItemKind::Enum(ast::EnumDef {
                name,
                variants,
                span,
                parameters,
            }),
            span,
        })
    }

    /// TODO dedupe with generics
    fn type_parameter_list(&mut self) -> Result<Option<Vec<ast::Ident>>> {
        debug!("type_parameter_list");
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
                Some(params)
            }
            _ => None,
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

    pub fn fn_def(&mut self) -> Result<ast::Function> {
        self.expect(TokenKind::Reserved(Keyword::Func))?;
        let lo = self.span;
        let name = self.ident()?;
        let generics = self.generics()?;
        let params = self.parse_fn_params()?;
        let return_ty = {
            if self.eat(TokenKind::Colon)? {
                Some(self.parse_type()?)
            } else {
                None
            }
        };
        let body = self.block()?;
        let span = lo.merge(self.span);
        // We don't currently parse this, hardcode false
        let is_async = false;
        Ok(ast::Function {
            name,
            params,
            body,
            return_ty,
            is_async,
            generics,
            span,
        })
    }

    fn component_def(&mut self) -> Result<ast::Component> {
        self.expect(TokenKind::Reserved(Keyword::Component))?;
        let lo = self.span;
        let name = self.ident()?;
        let generics = self.generics()?;
        let params = self.parse_fn_params()?;
        let return_ty = {
            if self.eat(TokenKind::Colon)? {
                Some(self.parse_type()?)
            } else {
                None
            }
        };
        let body = self.block()?;
        let span = lo.merge(self.span);
        Ok(ast::Component {
            name,
            params,
            body,
            return_ty,
            generics,
            is_async: false,
            span,
        })
    }

    fn parse_struct(&mut self) -> Result<ast::Item> {
        let struct_def = self.struct_def()?;
        let span = struct_def.span;
        Ok(ast::Item {
            kind: ast::ItemKind::Struct(struct_def),
            span,
        })
    }

    fn struct_def(&mut self) -> Result<ast::Struct> {
        use Keyword::Struct;
        use TokenKind::{Colon, Ident, LCurlyBrace, RCurlyBrace, Reserved};
        self.expect(Reserved(Struct))?;
        let span = self.span;
        let name = self.ident()?;
        let parameters = self.type_parameter_list()?;
        self.expect(LCurlyBrace)?;

        let mut fields: Vec<ast::StructField> = vec![];

        loop {
            match self.peek()?.kind {
                Ident(_) => {
                    let name = self.ident()?;
                    self.expect(Colon)?;
                    let ty = self.parse_type()?;
                    let field = ast::StructField { name, ty };
                    fields.push(field);
                }
                _ => break,
            }
        }
        self.expect(RCurlyBrace)?;
        Ok(ast::Struct {
            name,
            span,
            parameters,
            fields,
        })
    }

    fn async_def(&mut self) -> Result<ast::Item> {
        let async_token = self.expect(TokenKind::Reserved(Keyword::Async))?;
        match self.peek()?.kind {
            TokenKind::Reserved(Keyword::Func) => self.parse_fn(true),
            TokenKind::Reserved(Keyword::Component) => {
                self.parse_component(true)
            }
            _ => Err(Diagnostic::error()
                .with_message(
                    "Only functions and components can de declared as async",
                )
                .with_labels(vec![Label::primary(async_token.span)])),
        }
    }

    /// Parse a function definition
    fn parse_fn(&mut self, is_async: bool) -> Result<ast::Item> {
        let mut fn_def = self.fn_def()?;
        fn_def.is_async = is_async;
        let span = fn_def.span;
        let kind = ast::ItemKind::Fn(fn_def);
        Ok(ast::Item {
            // TODO
            kind,
            span,
        })
    }

    fn parse_component(&mut self, is_async: bool) -> Result<ast::Item> {
        let mut component_def = self.component_def()?;
        component_def.is_async = is_async;
        let span = component_def.span;
        let kind = ast::ItemKind::Component(component_def);
        Ok(ast::Item { kind, span })
    }

    /// Parse a list of function parameters
    fn parse_fn_params(&mut self) -> Result<ast::ParamType> {
        debug!("parse_fn_params");
        use TokenKind::{Comma, Ident, LBrace, LCurlyBrace, RParen};
        let mut params = vec![];
        // Argument list is optional for functions
        // that take no arguments
        if !self.eat(TokenKind::LParen)? {
            return Ok(ast::ParamType::Empty);
        }
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
                    return Err(self.fatal(
                        "Expected a funciton paramter",
                        "Found this shit",
                        self.span,
                    ));
                }
            }
        }
        self.expect(TokenKind::RParen)?;
        match params.len() {
            0 => Ok(ast::ParamType::Empty),
            1 => {
                let param = params.get(0).expect("proven");
                // TODO move memory out of params so we dont clone?
                Ok(ast::ParamType::Single(param.clone()))
            }
            _ => Ok(ast::ParamType::Multi(params)),
        }
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
                    Some(self.parse_type()?)
                }
                // If no annotation is provided we assume the type
                // must be inferred.
                _ => None,
            }
        };
        let span = lo.merge(self.span);
        Ok(ast::Param {
            local,
            ty,
            id: DUMMY_NODE_ID,
            span,
        })
    }

    pub fn block(&mut self) -> Result<ast::Block> {
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

    pub(crate) fn stmt_list(&mut self) -> Result<Vec<ast::Stmt>> {
        // let was_newline_significant = self.is_newline_significant;
        let mut stmts = vec![];
        // let mut terminated = false;
        while !self.peek()?.follows_item_list() {
            // if terminated {
            //     return Err(self.fatal(
            //         "Expected a semicolon",
            //         "Only the last statement in a list may omit the semicolon",
            //         self.span,
            //     ));
            // }
            let stmt = self.stmt()?;

            // Newlines are delimeters
            self.eat(TokenKind::Newline)?;

            stmts.push(stmt);
        }
        // self.is_newline_significant = was_newline_significant;
        Ok(stmts)
    }

    pub fn stmt(&mut self) -> Result<ast::Stmt> {
        let was_newline_signfificant = self.is_newline_significant;
        // self.is_newline_significant = false;
        let token = self.peek()?;
        let stmt = match token.kind {
            TokenKind::Newline => {
                // Ignore
                self.skip()?;
                self.stmt()
            }
            TokenKind::Reserved(Keyword::Let) => {
                self.skip()?;
                let local = self.local()?;
                let span = local.span;
                stmt(ast::StmtKind::Local(Box::new(local)), span)
            }
            TokenKind::Reserved(Keyword::State) => {
                self.skip()?;
                let local = self.local()?;
                let span = local.span;
                stmt(ast::StmtKind::State(Box::new(local)), span)
            }
            // Return statement
            TokenKind::Reserved(Keyword::Return) => {
                self.expect(TokenKind::Reserved(Keyword::Return))?;
                let lo = self.span;
                let expr = self.expr(Precedence::NONE)?;
                let span = lo.merge(expr.span);
                stmt(ast::StmtKind::Return(Box::new(expr)), span)
            }
            // Throw statement
            TokenKind::Reserved(Keyword::Throw) => {
                self.expect(TokenKind::Reserved(Keyword::Throw))?;
                let lo = self.span;
                let expr = self.expr(Precedence::NONE)?;
                let span = lo.merge(self.span);
                stmt(ast::StmtKind::Throw(expr), span)
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
                        return Err(self.fatal("", "", self.span));
                    }
                };
                let catch_block = self.block()?;
                let span = lo.merge(self.span);
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
              //     Err(self.fatal("Unsupported statement", "here", self.span))
              // }
        };
        self.is_newline_significant = was_newline_signfificant;
        stmt
    }

    fn array_pattern(&mut self) -> Result<ast::LocalPattern> {
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
                    return Err(self.fatal(
                        "Unexpected token",
                        "Unexpected",
                        self.span,
                    ));
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
                Comma | RCurlyBrace => {
                    ast::LocalPattern::Ident(key.clone(), key.span)
                }
                // Everything else is a syntax error
                _ => {
                    return Err(self.fatal(
                        "Unexpected token",
                        "Unexpected",
                        self.span,
                    ))
                }
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
                _ => {
                    return Err(self.fatal(
                        "Unexpected token",
                        "Unexpected",
                        self.span,
                    ))
                }
            }
        }
        self.expect(RCurlyBrace)?;
        let span = lo.merge(self.span);
        Ok(ast::LocalPattern::Object(patterns, span))
    }

    fn local_pattern(&mut self) -> Result<ast::LocalPattern> {
        use TokenKind::{Ident, LBrace, LCurlyBrace, Reserved};
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
            _ => {
                let token = self.next_token()?;
                let span = self.span;
                Err(self.fatal(
                    "Syntax error",
                    &format!("did not expect {}", token.kind),
                    span,
                ))
            }
        }
    }

    fn local(&mut self) -> Result<ast::Local> {
        let lo = self.span;
        let pattern = self.local_pattern()?;
        // Optional type annotation
        let ty = if self.eat(TokenKind::Colon)? {
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Equals)?;
        // TODO init is optional?
        let init = self.expr(Precedence::NONE)?;
        let span = lo.merge(self.span);
        Ok(ast::Local {
            id: DUMMY_NODE_ID,
            name: pattern,
            ty,
            // Optional initializing expression.
            init: Some(Box::new(init)),
            span,
        })
    }

    pub(crate) fn expr(&mut self, precedence: Precedence) -> Result<ast::Expr> {
        debug!("expr (precedence: {:?})", precedence);
        let mut expr = self.prefix_expr()?;
        debug!("prefix expr: {:?}", expr);

        // Newlines are significant after prefix expressions.
        // Except in the case where we *know* there is going to be
        // a line terminator.
        self.is_newline_significant = true;
        if self.peek()?.kind == TokenKind::Newline
            && self.expr_terminator.is_none()
        {
            self.is_newline_significant = false;
            return Ok(expr);
        }
        self.is_newline_significant = false;

        while precedence < self.peek_precedence()? {
            expr = self.infix_expr(expr)?;
        }
        Ok(expr)
    }

    // Expressions that begin with an identifier
    fn ident_expr(&mut self) -> Result<ast::Expr> {
        let ident = self.ident()?;
        let span = ident.span;
        ast::expr(ast::ExprKind::Reference(ident), span)
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
                return Err(self.fatal(
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
        ast::expr(kind, span)
    }

    fn prefix_expr(&mut self) -> Result<ast::Expr> {
        let is_sig = self.is_newline_significant;
        let token = self.peek()?;
        debug!("prefix_expr: {:?}, {}", token, is_sig);
        match token.kind {
            // Literal values such as numbers, strings, booleans
            TokenKind::Literal(_) => {
                let lit = self.parse_lit()?;
                let span = lit.span;
                ast::expr(ast::ExprKind::Lit(lit), span)
            }
            // Array literals
            TokenKind::LBrace => {
                self.expect(TokenKind::LBrace)?;
                let lo = self.span;
                let exprs = self.expr_list(TokenKind::RBrace)?;
                let span = lo.merge(self.span);
                ast::expr(ast::ExprKind::Array(exprs), span)
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
                ast::expr(ast::ExprKind::Unary(op, Box::new(expr)), span)
            }
            // Block expression OR object literal?
            TokenKind::LCurlyBrace => {
                // self.obj_expr()
                let block = self.block()?;
                let span = block.span;
                ast::expr(ast::ExprKind::Block(Box::new(block)), span)
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
            TokenKind::Reserved(Keyword::If) => {
                let if_expr = self.if_expr()?;
                let span = if_expr.span;
                ast::expr(ast::ExprKind::If(if_expr), span)
            }
            // For expression
            TokenKind::Reserved(Keyword::For) => self.for_expr(),
            // Tempalte expression
            TokenKind::LessThan => {
                let template = self.template()?;
                let span = template.span;
                ast::expr(ast::ExprKind::Template(template), span)
            }
            // Function expression
            TokenKind::Reserved(Keyword::Func) => {
                let fn_def = self.fn_def()?;
                let span = fn_def.span;
                ast::expr(ast::ExprKind::Func(Box::new(fn_def)), span)
            }
            // Lambda function expression
            TokenKind::BinOr | TokenKind::Or => {
                let lambda = self.lambda()?;
                let span = lambda.span;
                ast::expr(ast::ExprKind::Lambda(lambda), span)
            }
            // Match expression
            TokenKind::Reserved(Keyword::Match) => self.match_expr(),
            _ => {
                self.skip()?;
                Err(self.fatal(
                    "Failed to parse an expression",
                    "We expected an expression here",
                    self.span,
                ))
            }
        }
    }

    fn lambda_params(&mut self) -> Result<(ast::ParamType, Span)> {
        use TokenKind::{BinOr, Comma, Ident, Or};
        if self.eat(Or)? {
            Ok((ast::ParamType::Empty, self.span))
        } else {
            self.expect(BinOr)?;
            let span = self.span;
            let mut params = vec![];
            loop {
                match self.peek()?.kind {
                    // End of param list
                    BinOr => {
                        break;
                    }
                    Ident(_) => {
                        let param = self.parse_fn_param()?;
                        params.push(param);
                        if !self.eat(Comma)? {
                            break;
                        }
                    }
                    _ => {
                        self.next_token()?;
                        return Err(self.fatal(
                            "Expected a funciton paramter",
                            "Found this shit",
                            self.span,
                        ));
                    }
                }
            }
            self.expect(BinOr)?;
            let params = match params.len() {
                0 => ast::ParamType::Empty,
                1 => {
                    let param = params.get(0).expect("proven");
                    // TODO move memory out of params so we dont clone?
                    ast::ParamType::Single(param.clone())
                }
                _ => ast::ParamType::Multi(params),
            };
            Ok((params, span))
        }
    }

    fn lambda(&mut self) -> Result<ast::Lambda> {
        let (params, lo) = self.lambda_params()?;
        let (body, span) = match self.peek()?.kind {
            TokenKind::LCurlyBrace => {
                let block = self.block()?;
                let span = block.span;
                let block = Box::new(block);
                (ast::LambdaBody::Block(block), span)
            }
            _ => {
                let expr = self.expr(Precedence::NONE)?;
                let span = expr.span;
                let expr = Box::new(expr);
                (ast::LambdaBody::Expr(expr), span)
            }
        };
        let span = lo.merge(span);
        Ok(ast::Lambda { body, params, span })
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
            name,
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
                    return Err(self.fatal(
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

                ast::expr(ast::ExprKind::Lit(lit), span)?
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
                return Err(self.fatal(
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
        use TokenKind::{
            Div, LCurlyBrace, LessThan, RCurlyBrace, TemplateText,
        };
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
                    let child =
                        ast::TemplateChild::Template(Box::new(template));
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
        ast::expr(
            ast::ExprKind::For(pattern, Box::new(expr), Box::new(block)),
            span,
        )
    }

    fn if_expr(&mut self) -> Result<ast::IfExpression> {
        use Keyword::{Else, If};
        use TokenKind::{LParen, RParen, Reserved};
        debug!("if_expr");
        self.expect(Reserved(If))?;
        let lo = self.span;
        // Disallow trailing closures in the condition
        let allow_trailing_closure = self.allow_trailing_closure;
        self.allow_trailing_closure = false;
        self.expect(LParen)?;
        let condition = self.expr(Precedence::NONE)?;
        self.expect(RParen)?;
        self.allow_trailing_closure = allow_trailing_closure;
        let value = self.expr(Precedence::NONE)?;

        let alternate = if self.eat(Reserved(Else))? {
            let alternate = if Reserved(If) == self.peek()?.kind {
                let if_expr = self.if_expr()?;
                ast::Else::IfExpression(if_expr)
            } else {
                let expr = self.expr(Precedence::NONE)?;
                ast::Else::Value(expr)
            };
            Box::new(alternate).into()
        } else {
            None
        };

        let span = lo.merge(self.span);

        let if_expr = ast::IfExpression {
            span,
            condition: condition.into(),
            body: value.into(),
            alternate,
        };

        Ok(if_expr)

        // if let TokenKind::Reserved(Keyword::Else) = self.peek()?.kind {
        //     // We have an alternate block
        // }
        // Ok(ast::IfExpr {
        //     span: lo.merge(self.span),
        //     condition: Box::new(condition),
        //     block: Box::new(block),
        //     alt,
        // })
    }

    fn expect_lit(&mut self) -> Result<token::Lit> {
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Literal(lit) => Ok(lit),
            _ => Err(self.fatal("Expected literal", "found this", token.span)),
        }
    }

    fn parse_lit(&mut self) -> Result<ast::Lit> {
        match self.peek()?.kind {
            // Literals such as numbers, strings, and booleans
            TokenKind::Literal(_) => {
                let token::Lit { kind, symbol } = self.expect_lit()?;
                use token::LitKind::{Bool, Number, Str};
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

            _ => Err(self.fatal("expected literal", "lol", self.span)),
        }
    }

    fn trailing_closure(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        let block = self.block()?;
        let span = left.span.merge(block.span);
        let kind = ast::ExprKind::TrailingClosure(left.into(), block);
        ast::expr(kind, span)
    }

    fn infix_expr(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        debug!("infix_expr: {:?}", left);
        use TokenKind::*;
        match self.peek()?.kind {
            // Binary
            Plus | Minus | Div | Mul | LessThan | GreaterThan | DblEquals
            | And | Or | Pipeline | BinOr => self.binary_expr(left),
            // Assignment
            Equals | PlusEquals => self.assign_expr(left),
            // Conditional
            Question => self.cond_expr(left),
            // Call
            LParen => self.call_expr(left),
            // Trailing closure
            LCurlyBrace => self.trailing_closure(left),
            // Member or await expressions
            Dot => self.dot_expr(left),
            // Dot => self.member_expr(left),
            QuestionDot => self.optional_member_expr(left),
            _ => {
                self.skip()?;
                Err(self.fatal("Unknown infix expression", "Here", self.span))
            }
        }
    }

    fn dot_expr(&mut self, left: ast::Expr) -> Result<ast::Expr> {
        use Keyword::Await;
        use TokenKind::{Dot, Ident, Reserved};
        self.expect(Dot)?;
        match self.peek()?.kind {
            Reserved(Await) => {
                self.skip()?;
                let span = left.span.merge(self.span);
                Ok(ast::Expr {
                    span,
                    kind: ast::ExprKind::Await(left.into()),
                })
                // AwaitExpression
            }
            Ident(_) => {
                let ident = self.ident()?;
                let span = left.span.merge(self.span);
                Ok(ast::Expr {
                    span,
                    kind: ast::ExprKind::Field(left.into(), ident),
                })
                // CallExpression or FieldExpression
            }
            _ => {
                panic!("oops dot")
                // Invalid
            }
        }
    }

    fn match_expr(&mut self) -> Result<ast::Expr> {
        debug!("match_expr");
        self.expect(TokenKind::Reserved(Keyword::Match))?;
        let lo = self.span;
        // TODO this seems like a hard-to-maintain pattern
        let allow_trailing_closure = self.allow_trailing_closure;
        self.allow_trailing_closure = false;
        let cond = self.expr(Precedence::NONE)?;
        self.allow_trailing_closure = allow_trailing_closure;
        self.expect(TokenKind::LCurlyBrace)?;
        let mut cases = vec![];
        loop {
            let case = self.match_arm_expr()?;
            cases.push(case);
            if self.peek()?.kind == TokenKind::RCurlyBrace {
                break;
            }
            if self.eat(TokenKind::Comma)? {
                continue;
            }
        }
        self.expect(TokenKind::RCurlyBrace)?;
        let kind = ast::ExprKind::Match(Box::new(cond), cases);
        let span = lo.merge(self.span);
        ast::expr(kind, span)
    }

    fn match_arm_expr(&mut self) -> Result<ast::MatchArm> {
        use ast::ExprKind;
        debug!("match_arm_expr");
        let pattern = self.pattern()?;
        //  TODO pattern
        self.expect(TokenKind::Arrow)?;
        let body = self.expr(Precedence::NONE)?;
        Ok(ast::MatchArm { pattern, body })
    }

    fn pattern(&mut self) -> Result<ast::Pattern> {
        let name = self.ident()?;
        let lo = self.span;

        let kind = match self.peek()?.kind {
            TokenKind::Dot => {
                // Member or MemberTuple
                self.skip()?;
                let enum_name = name;
                let name = self.ident()?;

                match self.peek()?.kind {
                    TokenKind::LParen => {
                        // MemberTuple
                        self.skip()?;
                        let mut idents = vec![];
                        loop {
                            match self.peek()?.kind {
                                TokenKind::Ident(_) => {
                                    let ident = self.ident()?;
                                    idents.push(ident);
                                }
                                TokenKind::LParen => {
                                    self.skip()?;
                                    break;
                                }
                                _ => break,
                            }

                            if self.eat(TokenKind::Comma)? {
                                continue;
                            }
                            if self.eat(TokenKind::LParen)? {
                                break;
                            }
                        }
                        self.expect(TokenKind::RParen)?;
                        ast::PatternKind::MemberTuple {
                            type_name: enum_name,
                            name,
                            values: idents,
                        }
                    }
                    _ => ast::PatternKind::MemberFiedless {
                        type_name: enum_name,
                        name,
                    },
                }
            }
            TokenKind::Arrow => ast::PatternKind::Fiedless { name },
            TokenKind::LParen => {
                todo!();
                // Tuple
            }
            _ => {
                return Err(Diagnostic::error()
                    .with_message("Unexpected token in pattern"))
            }
        };

        debug!("kind: {:#?}", kind);

        Ok(ast::Pattern {
            kind,
            span: lo.merge(self.span),
        })
    }

    fn member_expr(&mut self, obj: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::Dot)?;
        let property = self.ident()?;
        let span = obj.span.merge(self.span);
        ast::expr(ast::ExprKind::Member(Box::new(obj), property), span)
    }

    fn optional_member_expr(&mut self, obj: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::QuestionDot)?;
        let property = self.ident()?;
        let span = obj.span.merge(self.span);
        ast::expr(ast::ExprKind::Member(Box::new(obj), property), span)
    }

    fn expr_list(&mut self, terminator: TokenKind) -> Result<Vec<ast::Expr>> {
        use TokenKind::Comma;
        let mut exprs = vec![];
        self.expr_terminator = Some(terminator.clone());
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
                self.expect(terminator)?;
                break;
            }
        }
        self.expr_terminator = None;
        Ok(exprs)
    }

    fn call_expr(&mut self, callee: ast::Expr) -> Result<ast::Expr> {
        debug!("CALL EXPR");
        #[derive(Debug, PartialEq, Eq)]
        enum CallFormat {
            Unknown,
            Named,
            Positional,
        };
        self.expect(TokenKind::LParen)?;
        let lo = callee.span;
        let mut arguments = vec![];
        let mut call_format = CallFormat::Unknown;

        // TODO dedupe
        if self.eat(TokenKind::RParen)? {
            let span = lo.merge(self.span);
            return ast::expr(
                ast::ExprKind::Call(Box::new(callee), arguments),
                span,
            );
        }

        self.expr_terminator = Some(TokenKind::Comma);

        loop {
            let expr = self.expr(Precedence::NONE)?;
            let lo = self.span;

            // Named arguments
            if self.eat(TokenKind::Colon)? {
                let colon_span = self.span;
                if call_format == CallFormat::Positional {
                    return Err(Diagnostic::error()
                        .with_message(
                            "Cannot mix positional and named arguments",
                        )
                        .with_labels(vec![Label::primary(expr.span)]));
                }

                // Make sure the expression we pased is a reference (ident)
                let ident = match expr.kind {
                    ast::ExprKind::Reference(ident) => ident,
                    _ => {
                        return Err(Diagnostic::error().with_message("Argument name must be an identifier").with_labels(vec![
                            Label::primary(expr.span),
                            Label::secondary(colon_span).with_message("We expect this to be a named argument because of this colon")
                        ]))
                    }
                };

                let value = self.expr(Precedence::NONE)?;
                let span = lo.merge(self.span);
                call_format = CallFormat::Named;
                arguments.push(ast::Argument {
                    name: Some(ident),
                    value,
                    span,
                });

            // args.push(expr);
            // These are named arguments now
            } else {
                if call_format == CallFormat::Named {
                    return Err(Diagnostic::error()
                        .with_message(
                            "Cannot mix positional and named arguments",
                        )
                        .with_labels(vec![Label::primary(expr.span)]));
                }
                call_format = CallFormat::Positional;
                let span = expr.span;
                arguments.push(ast::Argument {
                    name: None,
                    value: expr,
                    span,
                });
            }

            if self.eat(TokenKind::Comma)? {
                continue;
            }

            if self.eat(TokenKind::RParen)? {
                break;
            }
        }
        self.expr_terminator = None;
        let span = lo.merge(self.span);
        ast::expr(ast::ExprKind::Call(Box::new(callee), arguments), span)
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
        ast::expr(kind, span)
    }

    fn cond_expr(&mut self, test: ast::Expr) -> Result<ast::Expr> {
        self.expect(TokenKind::Question)?;
        let consequent = self.expr(Precedence::NONE)?;
        self.expect(TokenKind::Colon)?;
        let alt = self.expr(Precedence::ASSIGNMENT)?;
        let span = test.span.merge(self.span);
        ast::expr(
            ast::ExprKind::Cond(
                Box::new(test),
                Box::new(consequent),
                Box::new(alt),
            ),
            span,
        )
    }
}

#[inline]
pub fn stmt(kind: ast::StmtKind, span: Span) -> Result<ast::Stmt> {
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
