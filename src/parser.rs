use crate::{
    ast,
    token::{
        Token,
        TokenType::{self, *},
    },
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

pub struct Parser<'a> {
    input: &'a str,
    pub tokens: Vec<Token<'a>>,
    /// Current token
    current: usize,
    saved: usize,
    warnings: Vec<Warning>,
    errors: Vec<Error>,
}

// TODO: Implement Pratt Parsing
// Current version uses recursive descent
impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: Token::from_input(input),
            current: 0,
            saved: 0,
            warnings: Vec::new(),
            errors: Vec::new(),
        }
    }

    // Error helpers

    // Call only when there's a fatal error
    fn error(&mut self, message: std::string::String) -> Error {
        let error = Error::Other(message);
        self.push_error(error.clone());
        return error;
    }

    fn push_error(&mut self, error: Error) {
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
        self.tokens[self.current]
    }

    // Consumes the token if it is present
    fn is_next(&mut self, expected: TokenType) -> bool {
        let is_next = self.peek().token_type == expected;
        if is_next {
            self.current += 1
        }
        return is_next;
    }

    fn next(&mut self) -> Token<'a> {
        let token = self.peek();
        self.current += 1;
        return token;
    }

    fn previous(&self) -> Token<'a> {
        self.tokens[self.current - 1]
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len() - 1
    }

    fn check(&self, expected: TokenType) -> bool {
        self.peek().token_type == expected
    }

    pub fn matches(&mut self, token_types: &[TokenType]) -> Option<Token<'a>> {
        for token_type in token_types {
            if self.check(*token_type) {
                println!("MATCHED: {token_type:?}");
                return Some(self.next());
            }
        }
        return None;
    }

    /// Saves the current token's position
    /// Used to enable backtracking
    fn save_current(&mut self) {
        self.saved = self.current;        
    }
    
    fn load_current(&mut self) {
        self.current = self.saved;
    }

    /// Consumes a token if it matches the expected token type
    /// Otherwise, returns an error.
    /// Does not consume the token if it does not match.
    fn consume(&mut self, expected: TokenType) -> Result<Token<'a>> {
        if self.check(expected) {
            Ok(self.next())
        } else {
            Err(Error::UnexpectedToken(expected, self.peek().token_type))
        }
    }

    // Combinators

    fn if_next<T>(
        &mut self,
        token: TokenType,
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
        closing: TokenType,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = Vec::new();

        while !self.is_next(closing) {
            list.push(item(self)?);
        }

        return Ok(list);
    }

    fn list1<T: 'a>(
        &mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: TokenType,
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
        separator: TokenType,
        item: fn(&mut Self) -> Result<T>,
        closing: TokenType,
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
        separator: TokenType,
        item: fn(&mut Self) -> Result<T>,
        closing: TokenType,
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
        closing: TokenType,
    ) -> Result<Vec<T>> {
        self.separated_list0(Comma, item, closing)
    }

    /// Comma separated list
    fn cs_list1<T: 'a>(
        &mut self,
        item: fn(&mut Self) -> Result<T>,
        closing: TokenType,
    ) -> Result<Vec<T>> {
        self.separated_list1(Comma, item, closing)
    }

    fn separated_while1<T: 'a>(
        &mut self,
        separator: TokenType,
        item: fn(&mut Self) -> Result<T>,
    ) -> Result<Vec<T>> {
        let mut list: Vec<T> = vec![item(self)?];

        while self.is_next(separator) {
            list.push(item(self)?);
        }

        return Ok(list);
    }

    fn block<T: 'a>(
        &mut self,
        statement: fn(&mut Self) -> Result<T>,
    ) -> Result<ast::StatementBlock<T>> {
        Ok(ast::StatementBlock {
            statements: self.after(LeftBrace)?.list0(statement, RightBrace)?,
        })
    }

    fn after(&mut self, expected: TokenType) -> Result<&mut Self> {
        self.consume(expected).map(|_| self)
    }

    // Get the parser back into a stable state after an error is found
    fn synchronize(&mut self) {
        todo!("synchronize")
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

        Ok(match self.next().token_type {
            Class => S::Class(self.class()?),
            Abstract => S::AbstractClass(self.abstract_class()?),
            Interface => S::Interface(self.interface()?),
            Mixin => S::Mixin(self.mixin()?),
            Extend => S::Extend(self.extend_with()?),
            Enum => S::Enum(self.enum_statement()?),
            Type => S::TypeDefinition(self.type_definition()?),
            Const => S::ConstantDefinition(self.constant()?),
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

    /// Parse generic parameters for a new type
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
        let item_type = if let Ok(named) = self.named_type() {
            if self.is_next(LeftBrace) {
                let body = self.block(Self::class_statement)?;
                let object = ast::ObjectType {
                    body,
                    supertype: Some(Box::new(named)),
                };
                ast::Type::Object(object)
            } else {
                named
            }
        } else {
            match self.peek().token_type {
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
                self.peek().token_type,
            ));
        }
    }

    fn identifier(&mut self) -> Result<ast::Identifier<'a>> {
        self.consume(TokenType::Identifier).map(ast::Identifier)
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

    fn number(&mut self, number: Token<'a>) -> Result<ast::Literal<'a>> {
        type CharMapper = fn(char, u32) -> Option<u32>;

        let (mapping, radix): (&fn(char, u32) -> Option<u32>, u64) = match number.token_type {
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
                    "Expected literal, got \"{}\".",
                    number.lexeme
                )));
            }
        };

        let lexeme = number.lexeme;
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
        while let Some(digit) = get_char(lexeme, self.current) {
            parsed_number = parsed_number
                .checked_mul(radix)
                .ok_or_else(|| Error::NumberTooLarge)?;

            parsed_number += mapping(digit, radix as u32).unwrap() as u64;
            current += 1;
        }

        return Ok(ast::Literal::Integer(parsed_number));
    }

    fn literal(&mut self) -> Result<ast::Literal<'a>> {
        use ast::Literal as Lit;

        Ok(match self.next().token_type {
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
                    self.primary_expression().ok().map(Box::new),
                ));
            }
            DotDotEquals => {
                return Ok(Lit::InclusiveRange(
                    None,
                    self.primary_expression().ok().map(Box::new),
                ));
            }
            // Todo: create a singular number token with a separate number type
            number_type => self.number(self.previous())?,
        })
    }

    fn assignment(&mut self) -> Result<ast::Assignment<'a>> {
        let possibly_invalid_token = self.peek();
        let target_expr = self.function_call()?;
        let value = self.after(Equals)?.expression()?;

        use ast::{AssignmentTarget::*, Expression as Expr};
        let target = match target_expr {
            Expr::Identifier(i) => Variable(i),
            Expr::Field(object, field) => Field(*object, field),
            Expr::Subscript { array, index } => Subscript {
                array: *array,
                index: *index,
            },
            Expr::Dereference(target) => Dereference(*target),
            _ => return Err(Error::InvalidLeftValue(possibly_invalid_token.token_type)),
        };

        return Ok(ast::Assignment { target, value });
    }

    fn return_statement(&mut self) -> Result<Option<ast::Expression<'a>>> {
        Ok(self.after(Return)?.expression().ok())
    }

    /// Does not consume tokens
    fn function_statement(&mut self) -> Result<ast::FunctionStatement<'a>> {
        use ast::FunctionStatement as Stmt;

        Ok(match self.peek().token_type {
            Let => Stmt::Let(self.let_variable()?),
            Forever => Stmt::Forever(self.forever_statement()?),
            While => Stmt::While(self.while_statement()?),
            For => Stmt::For(self.for_statement()?),
            Return => Stmt::Return(self.return_statement()?),
            Break => Stmt::Break(self.break_statement()?),
            Continue => Stmt::Continue(self.continue_statement()?),
            Identifier => {
                // Needed because `assignment` consumes an expression
                self.save_current();
                if let Ok(assignment) = self.assignment() {
                    Stmt::Assignment(assignment)
                } else {
                    self.load_current();
                    Stmt::Expression(self.expression()?)
                }
            }
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
        let mut fields = Vec::new();

        loop {
            let field = self.identifier()?;

            let value = if self.is_next(Equals) {
                self.expression()?
            } else {
                ast::Expression::Identifier(field.clone())
            };

            fields.push((field, value));

            if self.is_next(Comma) {
                break;
            }
        }

        self.consume(RightBrace)?;
        return Ok(fields);
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

        Ok(match self.next().token_type {
            Identifier => Stmt::Field(self.field()?),
            Type => {
                let name = self.identifier()?;
                if self.is_next(Equals) {
                    let item_type = self.parse_type()?;
                    Stmt::TypeDefinition { name, item_type }
                } else {
                    Stmt::TypeRequirement(name)
                }
            }
            Fn => Stmt::Method(self.function()?),
            Static => match self.next().token_type {
                Fn => Stmt::StaticMethod(self.function()?),
                Identifier => self.partial_static_field()?,
                unexpected => {
                    return Err(Error::UnexpectedToken(TokenType::Identifier, unexpected));
                }
            },
            unexpected => {
                return Err(Error::UnexpectedToken(TokenType::Identifier, unexpected));
            }
        })
    }

    fn class_statement(&mut self) -> Result<ast::ClassStatement<'a>> {
        use ast::ClassStatement as Stmt;

        Ok(match self.next().token_type {
            Identifier => Stmt::Field(self.field()?),
            Type => Stmt::TypeDefinition(self.type_definition()?),
            Fn => Stmt::Method(self.function()?),
            Static => match self.next().token_type {
                Fn => Stmt::StaticMethod(self.function()?),
                Identifier => Stmt::StaticField(self.static_field()?),
                unexpected => return Err(Error::UnexpectedToken(Identifier, unexpected)),
            },
            unexpected => return Err(Error::UnexpectedToken(Identifier, unexpected)),
        })
    }

    /// Assumes `Identifier` was consumed
    fn field(&mut self) -> Result<ast::Field<'a>> {
        Ok(ast::Field {
            name: ast::Identifier(self.previous()),
            item_type: self.after(Colon)?.parse_type()?,
        })
    }

    /// Assumes `Identifier` was consumed
    fn partial_static_field(&mut self) -> Result<ast::InterfaceStatement<'a>> {
        use ast::InterfaceStatement as Stmt;

        let name = ast::Identifier(self.previous());
        let item_type = self.after(Colon)?.parse_type()?;

        if self.is_next(Equals) {
            return Ok(Stmt::StaticDefinition(ast::StaticField {
                name,
                item_type,
                value: self.expression()?,
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
                self.peek().token_type,
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
                    self.peek().token_type,
                ));
            }

            if !self.is_next(Comma) {
                break;
            }
        }

        return Ok(block);
    }

    fn forever_statement(&mut self) -> Result<ast::Block<'a>> {
        self.after(Forever)?.block(Self::function_statement)
    }

    fn while_statement(&mut self) -> Result<ast::WhileStatement<'a>> {
        Ok(ast::WhileStatement {
            condition: self.after(While)?.expression()?,
            body: self.block(Self::function_statement)?,
            else_branch: self.if_next(Else, |p| p.block(Self::function_statement))?,
        })
    }

    fn for_statement(&mut self) -> Result<ast::ForStatement<'a>> {
        // Kinda cursed but still looks nice
        self.consume(For)?;

        if let Ok(loop_variable) = self.match_clause() {
            if self.is_next(In) {
                return Ok(ast::ForStatement {
                    loop_variable: Some(loop_variable),
                    iterator: self.expression()?,
                    body: self.block(Self::function_statement)?,
                });
            } else {
                return Ok(ast::ForStatement {
                    loop_variable: None,
                    iterator: loop_variable
                        .as_literal()
                        .map(ast::Expression::Literal)
                        .ok_or_else(|| Error::Other("Invalid literal in for loop.".to_owned()))?,
                    body: self.block(Self::function_statement)?,
                });
            }
        }

        return Ok(ast::ForStatement {
            loop_variable: None,
            iterator: self.expression()?,
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

    // Currently uses recursive descent
    pub fn expression(&mut self) -> Result<ast::Expression<'a>> {
        // TODO: fix stack overflow
        self.or()
    }

    fn or(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.and()?;

        while let Ok(operator) = self.consume(Or) {
            let right = self.and()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn and(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.equality()?;

        while let Ok(operator) = self.consume(And) {
            let right = self.equality()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn equality(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.comparison()?;

        while let Some(operator) = self.matches(&[BangEquals, EqualsEquals]) {
            let right = self.comparison()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn comparison(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.term()?;

        // Todo: handle "<<" and ">>" bit shift operators
        while let Some(operator) = self.matches(&[Greater, GreaterEquals, Less, LessEquals]) {
            let right = self.term()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn term(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.factor()?;
        println!("Consumed expr: {expr:?}");

        while let Some(operator) = self.matches(&[Minus, Plus]) {
            let right = self.factor()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn factor(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.left_unary()?;
        println!("Factor: {expr:?}");

        while let Some(operator) = self.matches(&[Slash, Star]) {
            let right = self.left_unary()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }

    fn left_unary(&mut self) -> Result<ast::Expression<'a>> {
        if let Some(operator) = self.matches(&[Not, Minus]) {
            let right = self.left_unary()?;
            return Ok(ast::Expression::Unary(operator, Box::new(right)));
        }

        // TODO: add right unary "?" try operator
        let fc =  self.function_call();
        println!("Left unary: {fc:?}");
        fc
    }

    fn function_call(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.primary_expression()?;
        println!("Primary: {expr:?}");

        loop {
            if self.is_next(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.is_next(Dot) {
                if let Ok(name) = self.identifier() {
                    expr = ast::Expression::Field(Box::new(expr), name);
                } else if self.is_next(Star) {
                    expr = ast::Expression::Dereference(Box::new(expr));
                } else {
                    return Err(Error::UnexpectedInContext(
                        "field access",
                        self.peek().token_type,
                    ));
                }
            } else {
                break;
            }
        }

        return Ok(expr);
    }

    fn finish_call(&mut self, expr: ast::Expression<'a>) -> Result<ast::Expression<'a>> {
        Ok(ast::Expression::FunctionCall {
            function: Box::new(expr),
            arguments: self.cs_list0(Self::expression, RightParen)?,
        })
    }

    fn primary_expression(&mut self) -> Result<ast::Expression<'a>> {
        use ast::Expression as Expr;
        
        println!("Called primary with {:?}", self.peek());

        let expr = if let Ok(name) = self.identifier() {
            let next_token = self.peek();
            println!("Next token should be: {next_token:?}");
            Expr::Identifier(name)
        } else {
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
        };

        use ast::Literal::{ExclusiveRange, InclusiveRange};
        let possible_range = if self.is_next(DotDot) {
            Expr::Literal(ExclusiveRange(
                Some(Box::new(expr)),
                self.primary_expression().ok().map(Box::new),
            ))
        } else if self.is_next(DotDotEquals) {
            Expr::Literal(InclusiveRange(
                Some(Box::new(expr)),
                self.primary_expression().ok().map(Box::new),
            ))
        } else {
            expr
        };

        println!("Returning from primary: {possible_range:?}");
        return Ok(possible_range);
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
        let value = match self.peek().token_type {
            Equals => {
                self.next();
                if let Ok(constant) = self.identifier() {
                    Value::NamedConstant(constant)
                } else {
                    let literal = self.literal()?;
                    // TODO: maybe just make the value a constant expression
                    Value::Value(literal)
                }
            }
            LeftParen => {
                self.next();
                Value::Tuple(self.tuple_type()?)
            }
            LeftBrace => {
                self.next();
                Value::Object(self.object_type()?)
            }
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
            if self.is_next(At) {
                Some(self.destructure()?)
            } else {
                None
            }
        } else {
            self.destructure().ok()
        };

        if name.is_some() || destructure.is_some() {
            item_type = self.if_next(Colon, Self::parse_type)?;
        }

        return Ok(ast::MatchClause {
            name,
            destructure,
            item_type,
            guard_clause: self.if_condition().ok(),
        });
    }

    fn if_condition(&mut self) -> Result<ast::Expression<'a>> {
        let invert_condition = if self.is_next(Unless) {
            true
        } else {
            self.consume(If)?;
            false
        };
        let lexeme = self.previous().lexeme;

        let mut condition = self.expression()?;
        if invert_condition {
            let not_token = Token {
                token_type: TokenType::Not,
                lexeme,
            };
            condition = ast::Expression::Unary(not_token, Box::new(condition));
        }

        return Ok(condition);
    }

    fn break_statement(&mut self) -> Result<ast::BreakStatement<'a>> {
        Ok(ast::BreakStatement {
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
        Ok(match self.peek().token_type {
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
    fn static_field(&mut self) -> Result<ast::StaticField<'a>> {
        Ok(ast::StaticField {
            name: ast::Identifier(self.previous()),
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
        self.list0(Self::field, RightBrace)
    }

    fn function_type(&mut self) -> Result<ast::FunctionType<'a>> {
        Ok(ast::FunctionType {
            parameters: self.after(LeftParen)?.tuple_type()?,
            return_type: self.if_next(Colon, Self::parse_type)?.map(Box::new),
        })
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
pub enum Error {
    /// It's actually okay when this happens
    None,
    NumberTooLarge,
    UnexpectedToken(TokenType, TokenType),
    UnexpectedInContext(&'static str, TokenType),
    InvalidLeftValue(TokenType),
    InvalidMatchClause,
    Other(std::string::String),
}

type Result<T> = std::result::Result<T, Error>;
