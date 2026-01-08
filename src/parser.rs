use std::{cell::{Cell, RefCell}, iter::Peekable};

use crate::{
    ast,
    scanner::Scanner,
    token::Token::{self, *}
};

/// Used to unwrap tuples with a single value
trait FirstOr<T> {
    /// Returns the first item if the array is of length 1.
    /// Otherwise returns the array wrapped to match `T`.
    fn first_or<F: FnMut(Self) -> T>(self, f: F) -> T
    where
        Self: Sized + Clone;
}

impl<T: Clone> FirstOr<T> for Vec<T> {
    fn first_or<F: FnMut(Self) -> T>(self, mut f: F) -> T {
        if self.len() == 1 {
            self[0].clone()
        } else {
            f(self)
        }
    }
}

// Note: I hate how rust prevents mutating the struct whenever any part of it is immutably borrowed.
// It makes it so hard to manage internal state, even when its as simple as passing references to tokens.
// I could wrap everything in a cell, but that's more trouble than it's worth.

type Lexer<'a> = logos::Lexer<'a, Token<'a>>;

pub struct Parser<'a> {
    pub lexer: RefCell<Peekable<Lexer<'a>>>,
    warnings: Vec<Warning>,
    errors: Vec<Error<'a>>,
}

// TODO: Implement Pratt Parsing
// Current version uses recursive descent
impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: RefCell::new(lexer.peekable()),
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    // Error helpers

    // Call only when there's a fatal error
    fn error(&mut self, message: std::string::String) -> Error<'a> {
        let error = Error::Other(message);
        self.push_error(error.clone());
        return error;
    }

    fn push_error(&mut self, error: Error<'a>) {
        self.errors.push(error);
    }

    fn had_error(&self) -> bool {
        !self.errors.is_empty()
    }

    fn warn(&mut self, message: std::string::String) {
        self.warnings.push(Warning::Other(message));
    }

    // Parser helpers

    pub fn peek(&self) -> Token<'a> {
        let mut lexer = self.lexer.borrow_mut();
        let token = lexer.peek();
        return token.unwrap_or(&Ok(Token::Eof)).unwrap_or(Token::Error);
    }

    // Consumes the token if it is present
    fn is_next(&self, expected: Token<'a>) -> bool {
        let is_next = self.peek() == expected;
        if is_next {
            self.lexer.borrow_mut().next();
        }
        return is_next;
    }

    fn next(&mut self) -> Token<'a> {
        self.lexer.borrow_mut().next().unwrap_or(Ok(Token::Eof)).unwrap_or(Token::Error)
    }

    /// Skips a token and returns Self.
    /// Use only when you know the desired token is next
    fn skip(&mut self) -> &mut Self {
        self.lexer.borrow_mut().next();
        return self;
    }

    fn is_at_end(&self) -> bool {
        self.lexer.borrow_mut().peek().is_none()
    }

    fn check(&mut self, expected: Token<'a>) -> bool {
        self.peek() == expected
    }

    pub fn matches(&mut self, tokens: &[Token<'a>]) -> Option<Token<'a>> {
        for &token in tokens {
            if self.check(token) {
                return Some(self.next());
            }
        }
        return None;
    }

    /// Consumes a token if it matches the expected token type
    /// Otherwise, returns an error.
    /// Does not consume the token if it does not match.
    fn consume(&mut self, expected: Token<'a>) -> Result<Token<'a>> {
        if self.check(expected) {
            Ok(self.next())
        } else {
            Err(Error::UnexpectedToken(expected, self.peek()))
        }
    }

    fn consume_if(&mut self, predicate: fn(&Token<'a>) -> bool) -> Result<Token<'a>> {
        if predicate(&self.peek()) {
            Ok(self.next())
        } else {
            Err(Error::None)
        }
    }

    // Combinators

    fn if_next<T>(
        &mut self,
        token: Token<'a>,
        then: fn(&mut Self) -> Result<T>,
    ) -> Result<Option<T>> {
        Ok(if self.is_next(token) {
            Some(then(self)?)
        } else {
            None
        })
    }

    fn list0<T: 'a>(
        &mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = Vec::new();

        while !self.is_next(closing) {
            list.push(item(self)?);
        }

        return Ok(list);
    }

    fn list1<T: 'a>(
        &'a mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = vec![item(self)?];

        while !self.is_next(closing) {
            list.push(item(self)?);
        }

        return Ok(list);
    }

    /// Consumes zero or more of a parsed item with a separator, followed by a closing token
    fn separated_list0<T: 'a>(
        &mut self,
        separator: Token<'a>,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = Vec::new();

        if !self.check(closing) {
            loop {
                list.push(item(self)?);

                if !self.is_next(separator) {
                    break;
                }
            }
        }

        self.consume(closing)?;
        return Ok(list);
    }

    /// Consumes one or more of a parsed item with a separator, followed by a closing token
    fn separated_list1<T: 'a>(
        &mut self,
        separator: Token<'a>,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = vec![item(self)?];

        while !self.is_next(closing) {
            self.consume(separator)?;
            list.push(item(self)?);
        }

        return Ok(list);
    }

    /// Comma separated list
    fn cs_list0<T: 'a>(
        &mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        self.separated_list0(Comma, item, closing)
    }

    /// Comma separated list
    fn cs_list1<T: 'a>(
        &mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: Token<'a>,
    ) -> Result<Vec<T>> {
        self.separated_list1(Comma, item, closing)
    }

    fn separated_while1<T: 'a>(
        &mut self,
        separator: Token<'a>,
        item: fn(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = vec![item(self)?];

        while self.is_next(separator) {
            list.push(item(self)?);
        }

        return Ok(list);
    }

    /// Consumes a block of arbitrary statements.
    /// A block is defined as code surrounded by braces.\
    /// Consumes `LeftBrace`.
    fn block<T: 'a>(
        &mut self,
        statement: fn(&mut Self) -> Result<T>,
    ) -> Result<ast::StatementBlock<T>> {
        Ok(ast::StatementBlock {
            statements: self.after(LeftBrace)?.list0(statement, RightBrace)?,
        })
    }

    fn function_body(&mut self) -> Result<ast::StatementBlock<ast::FunctionStatement<'a>>> {
        self.block(Self::function_statement)
    }

    fn after(&mut self, expected: Token<'a>) -> Result<&mut Self> {
        self.consume(expected).map(|_| self)
    }

    fn before<T: 'a>(
        &mut self,
        expected: Token<'a>,
        before: fn(&mut Self) -> Result<T>,
    ) -> Result<T> {
        let item = before(self)?;
        self.consume(expected)?;
        return Ok(item);
    }

    // Get the parser back into a stable state after an error is found
    fn synchronize(&mut self) {
        self.next();
        while !self.is_at_end() {
            if matches!(
                self.peek(),
                Class
                    | Interface
                    | Abstract
                    | Mixin
                    | Extend
                    | Static
                    | Fn
                    | Let
                    | Const
                    | Do
                    | If
                    | Unless
                    | While
                    | Until
                    | For
                    | Loop
                    | Return
            ) {
                return;
            }

            self.next();
        }
    }

    // Parsing functions

    pub fn parse(&mut self) -> Vec<ast::Statement<'a>> {
        // Assuming every 32 tokens is a top-level statement
        let mut statements = Vec::with_capacity(self.tokens.len() / 32);

        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    eprintln!("Error: {err:?}");
                    self.push_error(err);
                    self.synchronize();
                }
            }
        }

        return statements;
    }

    fn statement(&mut self) -> Result<ast::Statement<'a>> {
        use ast::Statement as S;

        Ok(match self.next() {
            Class => S::Class(self.class()?),
            Abstract => S::AbstractClass(self.abstract_class()?),
            Interface => S::Interface(self.interface()?),
            Mixin => S::Mixin(self.mixin()?),
            Extend => S::Extend(self.extend_with()?),
            Enum => S::Enum(self.enum_statement()?),
            Type => S::TypeDefinition(self.type_definition()?),
            Const => S::Constant(self.constant()?),
            Use => S::Use(self.use_statement()?),
            Fn => S::Function(self.function()?),
            Eof => S::Eof,
            // Note: for macros, everything that's some kind of statement is going to need to check when a macro is invoked,
            // And hand control over to it instead.
            // Note: private modules/namespaces may be added later
            unexpected => return Err(Error::UnexpectedInContext("statement", unexpected)),
        })
    }

    fn class(&mut self) -> Result<ast::Class<'a>> {
        Ok(ast::Class {
            name: self.identifier()?,
            generics: self.generic_params()?,
            parent: self.if_next(Of, Self::parse_type)?,
            with: self
                .if_next(With, |p| p.separated_while1(Comma, Self::named_type))?
                .unwrap_or_else(Vec::new),
            body: self.block(Self::class_statement)?,
        })
    }

    /// Consumes `Less` if found, and returns an empty vec if not.
    /// Parse generic parameters for a new type.
    fn generic_params(&mut self) -> Result<Vec<ast::Generic<'a>>> {
        if self.is_next(Less) {
            self.cs_list1(Self::generic_param, Greater)
        } else {
            Ok(vec![])
        }
    }

    fn generic_param(&mut self) -> Result<ast::Generic<'a>> {
        Ok(ast::Generic {
            name: self.identifier()?,
            supertype: self.if_next(Colon, Self::parse_type)?,
            default_type: self.if_next(Equals, Self::parse_type)?,
        })
    }

    /// Parse a list of types as arguments to a generic
    fn generic_arguments(&mut self) -> Result<Vec<ast::Type<'a>>> {
        if self.is_next(Less) {
            self.cs_list1(Self::parse_type, Greater)
        } else {
            Ok(vec![])
        }
    }

    fn parse_type(&mut self) -> Result<ast::Type<'a>> {
        self.type_union()
    }

    fn type_union(&mut self) -> Result<ast::Type<'a>> {
        Ok(self
            .separated_while1(Pipe, Self::type_intersection)?
            .first_or(ast::Type::Union))
    }

    fn type_intersection(&mut self) -> Result<ast::Type<'a>> {
        Ok(self
            .separated_while1(Ampersand, Self::type_literal)?
            .first_or(ast::Type::Union))
    }

    fn type_literal(&mut self) -> Result<ast::Type<'a>> {
        // TODO: Fix parsing
        let item_type = if let Ok(named) = self.named_type() {
            named
        } else {
            match self.peek() {
                Fn => {
                    self.next();
                    ast::Type::Function(self.function_type()?)
                }
                Ampersand => {
                    self.next();
                    ast::Type::Reference(Box::new(self.type_literal()?))
                }
                Star => {
                    self.next();
                    ast::Type::Pointer(Box::new(self.type_literal()?))
                }
                LeftParen => {
                    self.next();
                    self.tuple_type()?.first_or(ast::Type::Tuple)
                }
                LeftBrace => {
                    self.next();
                    let body = self.block(Self::class_statement)?;
                    let object = ast::ObjectType {
                        body,
                        supertype: None,
                    };
                    ast::Type::Object(object)
                }
                _ => ast::Type::Literal(self.literal()?),
            }
        };

        return Ok(self.wrap_array_type(item_type)?);
    }

    fn wrap_array_type(&mut self, item_type: ast::Type<'a>) -> Result<ast::Type<'a>> {
        Ok(if self.is_next(LeftBracket) {
            if !self.check(RightBracket) {
                let expr = self.expression()?;
                let array = ast::Type::Array(Box::new(item_type), expr);
                self.wrap_array_type(array)?
            } else {
                let slice = ast::Type::Slice(Box::new(item_type));
                self.wrap_array_type(slice)?
            }
        } else {
            item_type
        })
    }

    fn named_type(&mut self) -> Result<ast::Type<'a>> {
        use ast::Type;

        if self.is_next(UppercaseSelf) {
            return Ok(Type::SelfKeyword {
                path: self.type_path(vec![])?,
                generics: self.generic_arguments()?,
            });
        } else if self.is_next(UppercaseSelf) {
            return Ok(Type::SuperKeyword {
                path: self.type_path(vec![])?,
                generics: self.generic_arguments()?,
            });
        } else if let Ok(name) = self.identifier() {
            return Ok(Type::Named {
                path: self.type_path(vec![name])?,
                generics: self.generic_arguments()?,
            });
        } else {
            return Err(Error::UnexpectedInContext(
                "named type",
                self.peek(),
            ));
        }
    }

    fn identifier(&mut self) -> Result<ast::Identifier<'a>> {
        let token = self.peek();
        if let Identifier(ident) = token {
            self.skip();
            Ok(ast::Identifier(ident))
        } else {
            Err(Error::ExpectedIdentifier(token))
        }
    }

    /// Assumes `Fn` is already parsed
    fn function_declaration(&mut self) -> Result<ast::FunctionDeclaration<'a>> {
        Ok(ast::FunctionDeclaration {
            name: self.identifier()?,
            generics: self.generic_params()?,
            parameters: self
                .after(LeftParen)?
                .cs_list0(Self::match_clause, RightParen)?,
            return_type: self.if_next(Colon, Self::parse_type)?,
        })
    }

    /// Does not assume `Let` is consumed
    fn let_variable(&mut self) -> Result<ast::LetStatement<'a>> {
        Ok(ast::LetStatement {
            clause: self.after(Let)?.match_clause()?,
            value: self.if_next(Equals, Self::expression)?,
        })
    }

    fn array_literal(&mut self) -> Result<Vec<ast::Expression<'a>>> {
        self.cs_list0(Self::expression, RightBracket)
    }

    // TODO: replace inside tokenizer
    fn number(&mut self, number: Token<'a>) -> Result<ast::Literal<'a>> {
/*         type CharMapper = fn(char, u32) -> Option<u32>;

        let (mapping, radix): (&fn(char, u32) -> Option<u32>, u64) = match number {
            Base64 => (&(base_64 as CharMapper), 64),
            Base36 => (&(char::to_digit as CharMapper), 36),
            Base32 => (&(char::to_digit as fn(char, u32) -> Option<u32>), 32),
            Hexadecimal => (&(char::to_digit as CharMapper), 16),
            Decimal => (&(char::to_digit as CharMapper), 10),
            Octal => (&(char::to_digit as CharMapper), 8),
            Quadal => (&(char::to_digit as CharMapper), 4),
            Binary => (&(char::to_digit as CharMapper), 2),
            _ => {
                return Err(Error::Other(format!(
                    "Expected literal, got \"{:?}\".",
                    number
                )));
            }
        };

        let lexeme = number;
        if lexeme.len() == 1 {
            let output = lexeme.chars().next().unwrap().to_digit(10).unwrap();
            return Ok(ast::Literal::Integer(output as u64));
        }

        let mut current = 0;
        if get_char(lexeme, 0) == Some('0') {
            current += 2;
        }

        // TODO: parse floating point numbers
        // Also maybe handle really large numbers correctly

        // Parse number
        let mut parsed_number = 0u64;
        while let Some(digit) = get_char(lexeme, current) {
            current += 1;
            if digit == '_' {
                continue;
            }

            parsed_number = parsed_number
                .checked_mul(radix)
                .ok_or_else(|| Error::NumberTooLarge)?;

            parsed_number += mapping(digit, radix as u32).unwrap() as u64;
        }

        return Ok(ast::Literal::Integer(parsed_number)); */
        todo!()
    }

    fn literal(&mut self) -> Result<ast::Literal<'a>> {
        use ast::Literal as Lit;

        Ok(match self.next() {
            Nil => Lit::Nil,
            False => Lit::False,
            True => Lit::True,
            String => self.parse_string(self.previous())?,
            LeftParen => Lit::Tuple(self.cs_list1(Self::expression, RightParen)?),
            LeftBracket => Lit::Array(self.array_literal()?),
            // Note: I was originally going to use braces for both blocks an objects, but now I might just add a new keyword
            LeftBrace => Lit::Object(self.object_literal()?),
            DotDot => {
                return Ok(Lit::ExclusiveRange(
                    None,
                    self.expression_binding(255).ok().map(Box::new),
                ));
            }
            DotDotEquals => {
                return Ok(Lit::InclusiveRange(
                    None,
                    self.expression_binding(255).ok().map(Box::new),
                ));
            }
            // Todo: create a singular number token with a separate number type
            _number_type => self.number(self.previous())?,
        })
    }

    /// Does not consume tokens
    fn function_statement(&mut self) -> Result<ast::FunctionStatement<'a>> {
        use ast::FunctionStatement as Stmt;

        Ok(match self.peek() {
            Use => Stmt::Use(self.skip().use_statement()?),
            Let => Stmt::Let(self.let_variable()?),
            _ => Stmt::Expression(self.expression()?),
        })
    }

    /// Assumes `Fn` is already consumed
    fn function(&mut self) -> Result<ast::FunctionDefinition<'a>> {
        Ok(ast::FunctionDefinition {
            declaration: self.function_declaration()?,
            body: self.block(Self::function_statement)?,
        })
    }

    fn object_literal(&mut self) -> Result<Vec<(ast::Identifier<'a>, ast::Expression<'a>)>> {
        self.cs_list0(Self::object_field, RightBrace)
    }

    fn object_field(&mut self) -> Result<(ast::Identifier<'a>, ast::Expression<'a>)> {
        let field = self.identifier()?;
        Ok((
            field,
            self.if_next(Equals, Self::expression)?
                .unwrap_or_else(|| ast::Expression::Identifier(field.clone())),
        ))
    }

    fn abstract_class(&mut self) -> Result<ast::AbstractClass<'a>> {
        Ok(ast::AbstractClass {
            name: self.after(Class)?.identifier()?,
            generics: self.generic_params()?,
            parent: self.if_next(Of, Self::parse_type)?,
            with: self
                .if_next(With, |p| p.separated_while1(Comma, Self::named_type))?
                .unwrap_or_else(Vec::new),
            body: self.block(Self::interface_statement)?,
        })
    }

    fn interface_statement(&mut self) -> Result<ast::InterfaceStatement<'a>> {
        use ast::InterfaceStatement as Stmt;

        Ok(match self.next() {
            Identifier(name) => Stmt::Field(self.field(name)?),
            Type => {
                let name = self.identifier()?;
                if self.is_next(Equals) {
                    let item_type = self.parse_type()?;
                    Stmt::TypeDefinition { name, item_type }
                } else {
                    Stmt::TypeRequirement(name)
                }
            }
            Fn => self.partial_function()?,
            Static => match self.next() {
                Fn => self.partial_function()?.to_static(),
                Identifier(name) => self.partial_static_field(name)?,
                unexpected => {
                    return Err(Error::UnexpectedToken(Token::Identifier("any"), unexpected));
                }
            },
            unexpected => {
                return Err(Error::UnexpectedToken(Token::Identifier("any"), unexpected));
            }
        })
    }

    fn partial_function(&mut self) -> Result<ast::InterfaceStatement<'a>> {
        let declaration = self.function_declaration()?;
        Ok(if self.peek() == RightBrace {
            ast::InterfaceStatement::Method(ast::FunctionDefinition {
                declaration,
                body: self.function_body()?,
            })
        } else {
            ast::InterfaceStatement::MethodRequirement(declaration)
        })
    }

    fn class_statement(&mut self) -> Result<ast::ClassStatement<'a>> {
        use ast::ClassStatement as Stmt;

        Ok(match self.next() {
            Identifier(name) => Stmt::Field(self.field(name)?),
            Type => Stmt::TypeDefinition(self.type_definition()?),
            Fn => Stmt::Method(self.function()?),
            Static => match self.next() {
                Fn => Stmt::StaticMethod(self.function()?),
                Identifier(name) => Stmt::StaticField(self.static_field(name)?),
                unexpected => return Err(Error::UnexpectedToken(Identifier("any"), unexpected)),
            },
            Class => Stmt::NestedClass(self.nested_class()?),
            unexpected => return Err(Error::UnexpectedToken(Identifier("any"), unexpected)),
        })
    }

    /// Assumes `Class` was consumed
    /// Nested classes directly inherit from the class they are contained in.
    /// Further processing needed
    fn nested_class(&mut self) -> Result<ast::NestedClass<'a>> {
        Ok(ast::NestedClass {
            name: self.identifier()?,
            generics: self.generic_params()?,
            with: self
                .if_next(With, |p| p.separated_while1(Comma, Self::named_type))?
                .unwrap_or_else(Vec::new),
            body: self.block(Self::class_statement)?,
        })
    }

    /// Assumes `Identifier` was consumed
    fn field(&mut self, name: &'a str) -> Result<ast::Field<'a>> {
        Ok(ast::Field {
            name: ast::Identifier(name),
            item_type: self.after(Colon)?.parse_type()?,
        })
    }

    /// Assumes `Identifier` was consumed
    fn partial_static_field(&mut self, name: &'a str) -> Result<ast::InterfaceStatement<'a>> {
        use ast::InterfaceStatement as Stmt;

        let name = ast::Identifier(name);
        let item_type = self.after(Colon)?.parse_type()?;

        if let Some(value) = self.if_next(Equals, Self::expression)? {
            return Ok(Stmt::StaticDefinition(ast::StaticField {
                name,
                item_type,
                value,
            }));
        }

        return Ok(Stmt::StaticRequirement(ast::PartialStaticField {
            name,
            item_type,
        }));
    }

    fn type_definition(&mut self) -> Result<ast::TypeDefinition<'a>> {
        Ok(ast::TypeDefinition {
            name: self.identifier()?,
            generics: self.generic_params()?,
            type_value: self.after(Equals)?.parse_type()?,
        })
    }

    fn use_statement(&mut self) -> Result<ast::UsePath<'a>> {
        use ast::UsePath;

        if let Ok(name) = self.identifier() {
            if self.is_next(As) {
                let alias = self.identifier()?;
                return Ok(ast::UsePath::IdentifierWithAlias(name, alias));
            } else if self.is_next(Dot) {
                let path = self.use_statement()?;
                return Ok(UsePath::Identifier(name, Box::new(path)));
            } else {
                return Ok(UsePath::Identifier(name, Box::new(UsePath::None)));
            }
        } else if self.is_next(LeftBrace) {
            let block = self.use_block()?;
            self.consume(RightBrace)?;
            return Ok(UsePath::Block(block));
        } else if self.is_next(Star) {
            return Ok(UsePath::Wildcard);
        } else {
            return Err(Error::UnexpectedInContext(
                "use statement",
                self.peek(),
            ));
        }
    }

    fn use_block(&mut self) -> Result<Vec<ast::UsePath<'a>>> {
        use ast::UsePath;

        let mut block = Vec::new();

        loop {
            if self.is_next(LowercaseSelf) {
                if self.is_next(As) {
                    block.push(UsePath::SelfWithAlias(self.identifier()?));
                } else {
                    block.push(UsePath::SelfKeyword);
                }
            } else if let Ok(name) = self.identifier() {
                if self.is_next(As) {
                    let alias = self.identifier()?;
                    block.push(UsePath::IdentifierWithAlias(name, alias));
                } else if self.is_next(Dot) {
                    let path = self.use_statement()?;
                    block.push(UsePath::Identifier(name, Box::new(path)));
                } else {
                    block.push(UsePath::Identifier(name, Box::new(UsePath::None)));
                }
            } else if self.is_next(Star) {
                block.push(UsePath::Wildcard);
            } else {
                return Err(Error::UnexpectedInContext(
                    "use block",
                    self.peek(),
                ));
            }

            if !self.is_next(Comma) {
                break;
            }
        }

        return Ok(block);
    }

    fn do_expression(&mut self) -> Result<ast::Block<'a>> {
        self.after(Do)?.block(Self::function_statement)
    }

    fn loop_expr(&mut self) -> Result<ast::Block<'a>> {
        self.after(Loop)?.block(Self::function_statement)
    }

    fn while_expr(&mut self) -> Result<ast::WhileExpression<'a>> {
        let invert_condition = if self.is_next(Until) {
            true
        } else {
            self.consume(While)?;
            false
        };

        let mut condition = self.expression()?;
        if invert_condition {
            condition = ast::Expression::Unary(Not, Box::new(condition));
        }

        return Ok(ast::WhileExpression {
            condition: Box::new(condition),
            body: self.block(Self::function_statement)?,
            else_branch: self.if_next(Else, |p| p.block(Self::function_statement))?,
        });
    }

    /// Consumes `For`
    fn for_expr(&mut self) -> Result<ast::ForExpression<'a>> {
        // Kinda cursed but still looks nice
        self.consume(For)?;

        if let Ok(loop_variable) = self.match_clause() {
            if self.is_next(In) {
                return Ok(ast::ForExpression {
                    loop_variable: Some(Box::new(loop_variable)),
                    iterator: Box::new(self.expression()?),
                    body: self.block(Self::function_statement)?,
                });
            } else {
                return Ok(ast::ForExpression {
                    loop_variable: None,
                    iterator: loop_variable
                        .as_literal()
                        .map(ast::Expression::Literal)
                        .map(Box::new)
                        .ok_or_else(|| Error::Other("Invalid literal in for loop.".to_owned()))?,
                    body: self.block(Self::function_statement)?,
                });
            }
        }

        return Ok(ast::ForExpression {
            loop_variable: None,
            iterator: Box::new(self.expression()?),
            body: self.block(Self::function_statement)?,
        });
    }

    /// Assumes the head of the path is already consumed
    fn type_path(
        &mut self,
        mut path: Vec<ast::Identifier<'a>>,
    ) -> Result<Vec<ast::Identifier<'a>>> {
        while self.is_next(Dot) {
            let name = self.identifier()?;
            path.push(name);
        }

        return Ok(path);
    }

    // Expressions

    pub fn expression(&mut self) -> Result<ast::Expression<'a>> {
        self.expression_binding(0)
    }

    fn primary_expression(&mut self) -> Result<ast::Expression<'a>> {
        use ast::Expression as Expr;

        Ok(match self.next() {
            Identifier(name) => Expr::Identifier(self.identifier()?),
            Match => Expr::Match(self.match_expression()?),
            If | Unless => Expr::If(self.if_expression()?),
            Do => Expr::Block(self.do_expression()?),
            While | Until => Expr::While(self.while_expr()?),
            For => Expr::For(self.for_expr()?),
            Loop => Expr::Loop(self.loop_expr()?),
            Return => {
                self.next();
                Expr::Return(self.expression().ok().map(Box::new))
            },
            Break => {
                self.next();
                Expr::Break(self.if_next(Colon, Self::identifier)?, self.expression().ok().map(Box::new))
            },
            Continue => {
                self.next();
                Expr::Continue(self.if_next(Colon, Self::identifier)?)
            },
            DotDot => todo!(),
            DotDotEquals => todo!(),
            unary @ (Not | Minus | Tilde) => {
                let bp = unary.prefix_binding_power().unwrap().1;
                let rhs = self.expression_binding(bp)?;
                Expr::Unary(self.next(), Box::new(rhs))
            }
            _ => {
                let literal = self.literal()?;
                
                if let ast::Literal::Tuple(ref tuple) = literal {
                    if tuple.len() == 1 {
                        tuple[0].clone()
                    } else {
                        Expr::Literal(literal)
                    }
                } else {
                    Expr::Literal(literal)
                }
            }
        })
    }

    fn expression_binding(&mut self, min_bp: u8) -> Result<ast::Expression<'a>> {
        use ast::Expression as Expr;

        let lhs_token = self.peek();
        let mut lhs = self.primary_expression()?;

        // TODO: handle range precedence
        use ast::Literal::{ExclusiveRange, InclusiveRange};
        lhs = if self.is_next(DotDot) {
            Expr::Literal(ExclusiveRange(
                Some(Box::new(lhs)),
                self.primary_expression().ok().map(Box::new),
            ))
        } else if self.is_next(DotDotEquals) {
            Expr::Literal(InclusiveRange(
                Some(Box::new(lhs)),
                self.primary_expression().ok().map(Box::new),
            ))
        } else {
            lhs
        };

        loop {
            let op = self.peek();
            if let Some((postfix_bp, ())) = op.postfix_binding_power() {
                if postfix_bp < min_bp {
                    break;
                }
                self.next();

                lhs = match op {
                    Question => ast::Expression::Try(Box::new(lhs)),
                    Bang => ast::Expression::TryErr(Box::new(lhs)),
                    LeftBracket => ast::Expression::Subscript {
                        array: Box::new(lhs),
                        index: Box::new(self.before(RightBracket, Self::expression)?),
                    },
                    LeftParen => ast::Expression::FunctionCall {
                        function: Box::new(lhs),
                        arguments: self.cs_list0(Self::expression, RightParen)?,
                    },
                    Dot => match self.next() {
                        Identifier(name) => {
                            ast::Expression::Field(Box::new(lhs), ast::Identifier(name))
                        }
                        Star => ast::Expression::Dereference(Box::new(lhs)),
                        unexpected => {
                            return Err(Error::UnexpectedInContext("Field access", unexpected));
                        }
                    },
                    _ => lhs,
                };
                continue;
            }

            if op.is_assignment_operator() {
                use ast::AssignmentTarget::*;

                let assignment_target = match lhs {
                    Expr::Identifier(id) => Identifier(id),
                    Expr::Field(object, field) => Field(*object, field),
                    Expr::Subscript { array, index } => Subscript {
                        array: *array,
                        index: *index,
                    },
                    Expr::Dereference(deref) => Dereference(*deref),
                    _ => return Err(Error::InvalidLeftValue(lhs_token)),
                };
                lhs = Expr::Assignment(
                    op,
                    Box::new(assignment_target),
                    Box::new(self.expression()?),
                );
            }

            if let Some((left_bp, right_bp)) = op.infix_binding_power() {
                if left_bp < min_bp {
                    break;
                }
                self.next();

                lhs = match op {
                    // Desugar into nested function call
                    ColonGreater => {
                        let Expr::FunctionCall {
                            function,
                            mut arguments,
                        } = self.expression_binding(right_bp)?
                        else {
                            return Err(Error::ExpectedFunctionCall);
                        };
                        arguments.insert(0, lhs);
                        Expr::FunctionCall {
                            function,
                            arguments,
                        }
                    }
                    ColonGreaterGreater => {
                        let Expr::FunctionCall {
                            function,
                            mut arguments,
                        } = self.expression_binding(right_bp)?
                        else {
                            return Err(Error::ExpectedFunctionCall);
                        };
                        arguments.push(lhs);
                        Expr::FunctionCall {
                            function,
                            arguments,
                        }
                    },
                    As => Expr::As(Box::new(lhs), Box::new(self.type_literal()?)),
                    Is => Expr::Is(Box::new(lhs), Box::new(self.type_literal()?)),
                    _ => Expr::Binary(op, Box::new(lhs), Box::new(self.expression_binding(right_bp)?))
                }
            }
        }

        return Ok(lhs);
    }

    // Does not assume `If` has been consumed
    fn if_expression(&mut self) -> Result<ast::IfExpression<'a>> {
        let if_block = ast::ConditionalBlock {
            condition: Box::new(self.if_condition()?),
            body: self.block(Self::function_statement)?,
        };

        let mut conditional_branches = vec![if_block];

        let mut else_branch = None;
        while self.is_next(Else) {
            let condition = self.if_condition();
            let body = self.block(Self::function_statement)?;
            if let Ok(condition) = condition {
                let else_if_block = ast::ConditionalBlock {
                    condition: Box::new(condition),
                    body,
                };
                conditional_branches.push(else_if_block);
            } else {
                else_branch = Some(body);
                break;
            }
        }

        return Ok(ast::IfExpression {
            conditional_branches,
            else_branch,
        });
    }

    fn parse_string(&self, string: Token<'a>) -> Result<ast::Literal<'a>> {
        use ast::InterpolatedStringPiece::*;

        // TODO: create a special scanner for a string that allows scanning both Eris statements and custom syntax
        let lexeme = string.lexeme;
        let mut pieces = Vec::with_capacity(2);

        let mut start = 1;
        let mut is_escape = false;
        for (i, c) in lexeme.chars().enumerate().skip(1) {
            if is_escape {
                // Todo: push escaped character/sequence
                is_escape = false;
                continue;
            }
            match c {
                '\\' => {
                    pieces.push(Literal(&lexeme[start..i]));
                    is_escape = true;
                }
                '"' => {
                    pieces.push(Literal(&lexeme[start..i]));
                }
                _ => {}
            }
            start = i;
        }

        return Ok(ast::Literal::String(ast::InterpolatedString {
            token: string,
            pieces,
        }));
    }

    fn interface(&mut self) -> Result<ast::Interface<'a>> {
        Ok(ast::Interface {
            name: self.identifier()?,
            generics: self.generic_params()?,
            parent: self.if_next(Of, Self::named_type)?,
            body: self.block(Self::interface_statement)?,
        })
    }

    fn mixin(&mut self) -> Result<ast::Mixin<'a>> {
        Ok(ast::Mixin {
            name: self.identifier()?,
            generics: self.generic_params()?,
            body: self.block(Self::class_statement)?,
        })
    }

    fn extend_with(&mut self) -> Result<ast::ExtendWith<'a>> {
        Ok(ast::ExtendWith {
            extend: self.parse_type()?,
            with: self.if_next(With, Self::named_type)?,
            body: self.block(Self::class_statement)?,
        })
    }

    fn enum_statement(&mut self) -> Result<ast::Enum<'a>> {
        Ok(ast::Enum {
            name: self.identifier()?,
            generics: self.generic_params()?,
            body: self
                .after(LeftBrace)?
                .cs_list0(Self::enum_value, RightBrace)?,
        })
    }

    fn enum_value(&mut self) -> Result<(ast::Identifier<'a>, ast::EnumValue<'a>)> {
        use ast::EnumValue as Value;

        let name = self.identifier()?;
        let value = match self.peek() {
            Equals => Value::Expression(self.skip().expression()?),
            LeftParen => Value::Tuple(self.skip().tuple_type()?),
            LeftBrace => Value::Object(self.skip().object_type()?),
            _ => Value::None,
        };

        return Ok((name, value));
    }

    fn constant(&mut self) -> Result<ast::ConstAssignment<'a>> {
        Ok(ast::ConstAssignment {
            name: self.identifier()?,
            item_type: self.after(Colon)?.parse_type()?,
            value: self.after(Equals)?.expression()?,
        })
    }

    fn match_clause(&mut self) -> Result<ast::MatchClause<'a>> {
        // Valid clause syntax:
        // "u8": both identifier and type. Will be resolved during analysis
        // "name: type"
        // "(de, structure)"
        // "(de, structure): (type, literal)"
        // "name @ destructure: type"
        // All can be followed up by "if" | "unless"
        let mut item_type = None;

        // Try to consume identifier as a type, since a valid identifier is a valid type
        let name = if let Ok(type_or_name) = self.parse_type() {
            if let ast::Type::Named {
                ref path,
                ref generics,
            } = type_or_name
            {
                if generics.len() == 0 && path.len() == 1 {
                    Some(path[0].clone())
                } else {
                    item_type = Some(type_or_name);
                    None
                }
            } else {
                return Err(Error::InvalidMatchClause);
            }
        } else {
            Some(self.identifier()?)
        };

        let destructure = if name.is_some() {
            self.if_next(At, Self::destructure)?
        } else {
            self.destructure().ok()
        };

        if name.is_some() || destructure.is_some() {
            item_type = self.if_next(Colon, Self::parse_type)?;
        }

        let guard_clause = if matches!(self.peek(), If | Unless) {
            Some(self.if_condition()?)
        } else {
            None
        };

        return Ok(ast::MatchClause {
            name,
            destructure,
            item_type,
            guard_clause,
        });
    }

    /// Requires `If` or `Unless` to not be consumed
    fn if_condition(&mut self) -> Result<ast::Expression<'a>> {
        let invert_condition = if self.is_next(Unless) {
            true
        } else {
            self.consume(If)?;
            false
        };

        let mut condition = self.expression()?;
        if invert_condition {
            condition = ast::Expression::Unary(Not, Box::new(condition));
        }

        return Ok(condition);
    }

    fn break_statement(&mut self) -> Result<ast::Break<'a>> {
        Ok(ast::Break {
            label: self.after(Break)?.if_next(Colon, Self::identifier)?,
            value: self.expression().ok(),
        })
    }

    fn continue_statement(&mut self) -> Result<Option<ast::Identifier<'a>>> {
        self.after(Continue)?.if_next(Colon, Self::identifier)
    }

    /// Does not assume the first token is consumed
    fn destructure(&mut self) -> Result<ast::Destructure<'a>> {
        use ast::Destructure as Des;
        // Valid destructuring syntax
        // [match_clause, b]
        // [a, ...b, c]: name after splat is optional
        // (match_clause, _)
        // { match_clause = field_name, field }
        Ok(match self.peek() {
            LeftParen => {
                self.next();
                Des::Tuple(self.cs_list0(Self::match_clause, RightParen)?)
            }
            LeftBracket => {
                self.next();
                Des::Array(self.cs_list0(Self::array_destructure, RightBracket)?)
            }
            LeftBrace => {
                self.next();
                Des::Object(self.cs_list0(Self::object_destructure, RightBrace)?)
            }
            _ => return Err(Error::None),
        })
    }

    fn array_destructure(&mut self) -> Result<ast::ArrayDestructure<'a>> {
        Ok(if self.is_next(DotDotDot) {
            ast::ArrayDestructure::Splat(self.match_clause().ok())
        } else {
            ast::ArrayDestructure::Plain(self.match_clause()?)
        })
    }

    fn object_destructure(&mut self) -> Result<ast::ObjectDestructure<'a>> {
        Ok(ast::ObjectDestructure {
            clause: self.match_clause()?,
            field: self.if_next(Equals, Self::identifier)?,
        })
    }

    // Assumes `Static Identifier` is already consumed
    fn static_field(&mut self, name: &'a str) -> Result<ast::StaticField<'a>> {
        Ok(ast::StaticField {
            name: ast::Identifier(name),
            item_type: self.after(Colon)?.parse_type()?,
            value: self.after(Equals)?.expression()?,
        })
    }

    /// Assumes "(" is already consumed
    fn tuple_type(&mut self) -> Result<Vec<ast::Type<'a>>> {
        self.cs_list0(Self::parse_type, RightParen)
    }

    /// Parses an object type literal.
    /// Assumes "{" is already consumed
    fn object_type(&mut self) -> Result<Vec<ast::Field<'a>>> {
        self.list0(|p| p.field(p.identifier()?.0), RightBrace)
    }

    fn function_type(&mut self) -> Result<ast::FunctionType<'a>> {
        Ok(ast::FunctionType {
            parameters: self.after(LeftParen)?.tuple_type()?,
            return_type: self.if_next(Colon, Self::parse_type)?.map(Box::new),
        })
    }
    
    fn match_expression(&mut self) -> Result<ast::MatchExpression<'a>> {
        Ok(ast::MatchExpression {
            input: Box::new(self.expression()?),
            match_arms: self.after(LeftBrace)?.cs_list1(Self::match_arm, RightBrace)?
        })
    }

    fn match_arm(&mut self) -> Result<(ast::MatchClause<'a>, ast::Expression<'a>)> {
        let clause = self.match_clause()?;
        self.consume(MinusArrow)?;
        let expr = if self.peek() == LeftBrace {
            ast::Expression::Block(self.function_body()?)
        } else {
            self.expression()?
        };

        return Ok((clause, expr));
    }
}

fn get_char(string: &str, index: usize) -> Option<char> {
    string
        .get(index..(index + 1))
        .and_then(|s| s.chars().next())
}

fn base_64(c: char, _: u32) -> Option<u32> {
    Some(match c {
        'A'..='Z' => (c as u32) - ('A' as u32),
        'a'..='z' => 26 + (c as u32) - ('a' as u32),
        '0'..='9' => 52 + (c as u32) - ('0' as u32),
        '+' => 62,
        '/' => 63,
        _ => return None,
    })
}

pub enum Warning {
    Other(std::string::String),
}

#[derive(Clone, Debug)]
pub enum Error<'a> {
    /// It's actually okay when this happens
    None,
    NumberTooLarge,
    UnexpectedToken(Token<'a>, Token<'a>),
    UnexpectedInContext(&'static str, Token<'a>),
    InvalidLeftValue(Token<'a>),
    InvalidMatchClause,
    Other(std::string::String),
    ExpectedFunctionCall,
    ExpectedIdentifier(Token<'a>),
}

type Result<'a, T> = std::result::Result<T, Error<'a>>;

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn expressions()
// }
