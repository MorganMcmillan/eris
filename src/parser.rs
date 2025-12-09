use either::Either::{self, Left, Right};

use crate::{ast, token::{Token, TokenType::{self, *}}};

// Note: I hate how rust prevents mutating the struct whenever any part of it is immutably borrowed.
// It makes it so hard to manage internal state, even when its as simple as passing references to tokens.
// I could wrap everything in a cell, but that's more trouble than it's worth.

pub struct Parser<'a> {
    input: &'a str,
    tokens: Vec<Token<'a>>,
    /// Current token
    current: usize,
    warnings: Vec<Warning>,
    errors: Vec<Error>
}

// TODO: Implement Pratt Parsing
// Current version uses recursive descent
impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            tokens: Token::from_input(input),
            current: 0,
            warnings: Vec::new(),
            errors: Vec::new()
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
    
    fn peek(&self) -> Token<'a> {
        self.tokens[self.current]
    }
    
    // Consumes the token if it is present
    fn is_next(&mut self, expected: TokenType) -> bool {
        let is_next = self.peek().token_type == expected;
        if is_next { self.current += 1 }
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
        self.peek().token_type == Eof
    }

    fn check(&self, expected: TokenType) -> bool {
        self.peek().token_type == expected
    }

    fn matches(&mut self, token_types: &[TokenType]) -> Option<Token<'a>> {
        for token_type in token_types {
            if self.check(*token_type) {
                return Some(self.next());
            }
        }
        return None;
    }
    
    /// Consumes a token if it matches the expected token type
    /// Otherwise, returns an error.
    /// Does not consume the token if it does not match.
    fn consume(&mut self, expected: TokenType) -> Result<Token<'a>> {
        if self.check(expected) {
            Ok(self.next())
        } else {
            Err(Error::UnexpectedToken(expected, self.previous().token_type))
        }
    }
    
    // Get the parser back into a stable state after an error is found
    fn synchronize(&mut self) {
        todo!()
    }

    // Parsing functions

    pub fn parse(&mut self) -> Vec<ast::Statement<'a>> {
        // Assuming every 32 tokens is a top-level statement
        let mut statements = Vec::with_capacity(self.tokens.len() / 32);
        
        while !self.is_at_end() {
            match self.statement() {
                Ok(statement) => statements.push(statement),
                Err(err) => {
                    self.push_error(err);
                    self.synchronize();
                },
            }
        }

        return statements;
    }

    fn statement(&mut self) -> Result<ast::Statement<'a>> {
        use ast::Statement as S;

        Ok (match self.next().token_type {
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
            // Note: for macros, everything that's some kind of statement is going to need to check when a macro is invoked,
            // And hand control over to it instead.
            // Note: private modules/namespaces may be added later
            _ => return Err(Error::Other("Expected statement, got WHATEVER".to_owned()))
        })
    }
    
    fn class(&mut self) -> Result<ast::Class<'a>> {
        let name = self.identifier()?;
        
        let generics = self.generic_params()?;

        let mut parent = None;
        if self.is_next(Of) {
            parent = Some(self.named_type()?);
        }

        let mut with = Vec::with_capacity(2);
        if self.is_next(With) {
            loop {
                with.push(self.named_type()?);
                if !self.is_next(Comma) {
                    break;
                }
            }
        }

        let body = self.class_body()?;

        return Ok(ast::Class {
            generics,
            name,
            parent,
            with,
            body
        })
    }

    /// Parse generic parameters for a new type
    fn generic_params(&mut self) -> Result<Vec<ast::Generic<'a>>> {
        let mut generics = Vec::new();
        if self.is_next(Less) {
            loop {
                let name = self.identifier()?;

                let mut supertype = None;
                if self.is_next(Colon) {
                    supertype = Some(self.parse_type()?);
                }

                let mut default_type = None;
                if self.is_next(Equals) {
                    default_type = Some(self.parse_type()?);
                }

                generics.push(ast::Generic {
                    name,
                    supertype,
                    default_type
                });

                if !self.is_next(Comma) {
                    break;
                }
            }
            self.consume(Greater)?;
        }

        return Ok(generics);
    }

    /// Parse a list of types as arguments to a generic
    fn generic_arguments(&mut self) -> Result<Vec<ast::Type<'a>>> {
        let mut generics = Vec::new();

        if self.is_next(Less) {
            self.type_list(&mut generics)?;
            
            self.consume(Greater);
        }
        
        return Ok(generics);
    }

    fn parse_type(&mut self) -> Result<ast::Type<'a>> {
        self.type_union()
    }

    fn type_union(&mut self) -> Result<ast::Type<'a>> {
        let mut types = vec![self.type_intersection()?];

        while self.is_next(Pipe) {
            types.push(self.type_intersection()?);
        }
        
        Ok(if types.len() == 1 {
            types[0].clone()
        } else {
            ast::Type::Union(types)
        })        
    }

    fn type_intersection(&mut self) -> Result<ast::Type<'a>> {
        let mut types = vec![self.type_literal()?];

        while self.is_next(Pipe) {
            types.push(self.type_literal()?);
        }
        
        Ok(if types.len() == 1 {
            types[0].clone()
        } else {
            ast::Type::Intersection(types)
        })        
    }

    fn type_literal(&mut self) -> Result<ast::Type<'a>> {
        let mut item_type = if let Ok(named) = self.named_type() {
            if self.is_next(LeftBrace) {
                let body = self.class_body()?;
                let object = ast::ObjectType {
                    body,
                    supertype: Some(Box::new(named))
                };
                ast::Type::Object(object)
            } else {
                named
            }
        } else {
            match self.peek().token_type {
                Fn => {
                    self.next();
                    ast::Type::Function(
                        self.function_type()?
                    )
                },
                Ampersand => {
                    self.next();
                    ast::Type::Reference(Box::new(
                        self.type_literal()?
                    ))
                },
                Star => {
                    self.next();
                    ast::Type::Pointer(Box::new(
                        self.type_literal()?
                    ))
                },
                LeftParen => {
                    self.next();
                    let tuple = self.tuple_type()?;
                    if tuple.len() == 1 {
                        tuple[0].clone()
                    } else {
                        ast::Type::Tuple(tuple)
                    }
                },
                LeftBrace => {
                    self.next();
                    let body = self.class_body()?;
                    let object = ast::ObjectType {
                        body,
                        supertype: None,
                    };
                    ast::Type::Object(object)
                },
                _ => ast::Type::Literal(self.literal()?)
            }
        };
        
        // TODO: wrap in range type

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
            let path = self.type_path(vec![])?;
            let generics = self.generic_arguments()?;

            return Ok(Type::SelfKeyword{
                path,
                generics
            });
        } else if let Ok(name) = self.identifier() {
            let path = self.type_path(vec![name])?;
            let generics = self.generic_arguments()?;

            return Ok(Type::Named {
                path,
                generics
            });
        } else {
            return Err(Error::UnexpectedInContext("named type", self.peek().token_type));
        }
    }

    // Currently uses recursive descent
    fn expression(&mut self) -> Result<ast::Expression<'a>> {
        self.or()
    }

    fn identifier(&mut self) -> Result<ast::Identifier<'a>> {
        self.consume(TokenType::Identifier).map(ast::Identifier)
    }

    fn function_declaration(&mut self) -> Result<ast::FunctionDeclaration<'a>> {
        self.consume(Fn)?;
        let name = self.identifier()?;
        let generics = self.generic_params()?;
                
        let mut parameters = Vec::new();
        self.consume(LeftParen);
        if !self.check(RightParen) {
            // TODO: perhaps allow consuming "else" to create a catch-all for functions of all arity-s
            loop {
                let parameter = self.match_clause()?;
                parameters.push(parameter);

                if !self.is_next(Comma) {
                    break ;
                }
            }
        }
        self.consume(RightParen);

        let mut return_type = None;
        if self.is_next(Colon) {
            return_type = Some(self.parse_type()?);
        }

        return Ok(ast::FunctionDeclaration {
            name,
            generics,
            parameters,
            return_type
        })
    }

    fn let_variable(&mut self) -> Result<ast::LetStatement<'a>> {
        self.consume(Let);
        let clause = self.match_clause()?;


        let mut value = None;
        if self.is_next(Equals) {
            value = Some(self.expression()?);
        }
        
        return Ok(ast::LetStatement {
            clause,
            value
        })
    }

    fn expression_list(&mut self) -> Result<Vec<ast::Expression<'a>>> {
        let mut expressions = Vec::new();

        loop {
            expressions.push(self.expression()?);
            
            if !self.is_next(Comma) {
                return Ok(expressions);
            }
        }
    }

    fn array_literal(&mut self) -> Result<Vec<ast::Expression<'a>>> {
        let array = if !self.check(RightBracket) {
            self.expression_list()?
        } else {
            vec![]
        };

        self.consume(RightBracket)?;
        return Ok(array);
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
                _ => return Err(Error::Other(format!("Expected primary expression, got \"{}\".", number.lexeme)))
        };
        
        let lexeme = number.lexeme;
        if lexeme.len() == 1 {
            let output = lexeme.chars()
                .next()
                .unwrap()
                .to_digit(10)
                .unwrap();
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
        Ok(match self.next().token_type {
            Nil => ast::Literal::Nil,
            False => ast::Literal::False,
            True => ast::Literal::True,
            String => self.parse_string(self.previous())?,
            LeftParen => {
                let tuple = self.expression_list()?;
                self.consume(RightParen);
                ast::Literal::Tuple(tuple)
            },
            LeftBracket => ast::Literal::Array(self.array_literal()?),
            // Note: I was originally going to use braces for both blocks an objects, but now I might just add a new keyword
            LeftBrace => ast::Literal::Object(self.object_literal()?),
            number_type => self.number(self.previous())?
        })
    }

    fn primary_expression(&mut self) -> Result<ast::Expression<'a>> {
        if let Ok(name) = self.identifier() {
            return Ok(ast::Expression::Identifier(name));
        }
        
        let literal = self.literal()?;
        if let ast::Literal::Tuple(ref tuple) = literal {
            if tuple.len() == 1 {
                return Ok(tuple[0].clone());
            }
        }

        return Ok(ast::Expression::Literal(literal));
    }

    fn finish_call(&mut self, expr: ast::Expression<'a>) -> Result<ast::Expression<'a>> {
        let mut arguments = Vec::new();

        if !self.check(RightParen) {
            loop {
                arguments.push(self.expression()?);

                if !self.is_next(Comma) {
                    break;
                }
            }
        }
        self.consume(RightParen)?;

        return Ok(ast::Expression::FunctionCall {
            function: Box::new(expr),
            arguments
        });
    }

    fn function_call(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.primary_expression()?;

        loop {
            if self.is_next(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.is_next(Dot) {
                if let Ok(name) = self.identifier() {
                    expr = ast::Expression::Field(Box::new(expr), name);
                } else if self.is_next(Star) {
                    expr = ast::Expression::Dereference(Box::new(expr));
                } else {
                    return Err(Error::UnexpectedInContext("field access", self.peek().token_type));
                }
            } else {
                break;
            }
        }
        
        return Ok(expr);
    }

    fn assignment(&mut self) -> Result<ast::Assignment<'a>> {
        let possibly_invalid_token = self.peek();
        let target_expr = self.function_call()?;
        self.consume(Equals)?;
        let value = self.expression()?;
        
        use ast::{Expression as Expr, AssignmentTarget::*};
        let target = match target_expr {
            Expr::Identifier(i) => Variable(i),
            Expr::Field(object, field) => Field(*object, field),
            Expr::Subscript { array, index } => Subscript {
                array: *array,
                index: *index
            },
            Expr::Dereference(target) => Dereference(*target),
            _ => return Err(Error::InvalidLeftValue(possibly_invalid_token.token_type))
        };
        
        return Ok(ast::Assignment {
            target,
            value
        });
    }

    fn return_statement(&mut self) -> Result<Option<ast::Expression<'a>>> {
        self.consume(Return)?;
        return Ok(self.expression().ok());
    }

    /// Does not consume tokens
    fn function_statement(&mut self) -> Result<ast::FunctionStatement<'a>> {
        use ast::FunctionStatement as Stmt;
        
        Ok(match self.peek().token_type {
            Let => Stmt::Let(self.let_variable()?),
            Forever => Stmt::Forever(self.forever()?),
            While => Stmt::While(self.while_statement()?),
            For => Stmt::For(self.for_statement()?),
            Return => Stmt::Return(self.return_statement()?),
            Break => Stmt::Break(self.break_statement()?),
            Continue => Stmt::Continue(self.continue_statement()?),
            Identifier => {
                if let Ok(assignment) = self.assignment() {
                    Stmt::Assignment(assignment)
                } else {
                    Stmt::Expression(self.expression()?)
                }
            }
            unexpected => return Err(Error::UnexpectedInContext("function statemetnt", unexpected))
        })
    }
    
    fn function_body(&mut self) -> Result<ast::Block<'a>> {
        self.consume(LeftBrace)?;

        let mut statements = Vec::new();
        while !self.check(RightBrace) {
            statements.push(self.function_statement()?);
        }
        self.consume(RightBrace)?;
        
        return Ok(ast::Block {
            statements
        })
    }
    
    /// Assumes `Fn` is already consumed
    fn function(&mut self) -> Result<ast::FunctionDefinition<'a>> {
        let declaration = self.function_declaration()?;
        let body = self.function_body()?;
        
        return Ok(ast::FunctionDefinition {
            declaration,
            body
        });
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
        return Ok(fields)
    }
    
    fn abstract_class(&mut self) -> Result<ast::AbstractClass<'a>> {
        self.consume(Class);

        let name = self.identifier()?;
        
        let generics = self.generic_params()?;

        let mut parent = None;
        if self.is_next(Of) {
            parent = Some(self.named_type()?);
        }

        let mut with = Vec::with_capacity(2);
        if self.is_next(With) {

            loop {
                with.push(self.named_type()?);
                if !self.is_next(Comma) {
                    break;
                }
            }
        }

        let body = self.interface_body()?;

        return Ok(ast::AbstractClass {
            generics,
            name,
            parent,
            with,
            body
        })
    }
    
    fn interface_body(&mut self) -> Result<ast::InterfaceBody<'a>> {
        self.consume(LeftBrace)?;

        let mut statements = Vec::new();
        while !self.is_next(RightBrace) {
            use ast::InterfaceStatement as Stmt;

            let statement = match self.next().token_type {
                Identifier => Stmt::Field(self.field()?),
                Type => {
                    let name = self.identifier()?;
                    if self.is_next(Equals) {
                        let item_type = self.parse_type()?;
                        Stmt::TypeDefinition { name, item_type }
                    } else {
                        Stmt::TypeRequirement(name)
                    }
                },
                Fn => Stmt::Method(self.function()?),
                Static => match self.next().token_type {
                    Fn => Stmt::StaticMethod(self.function()?),
                    // TODO: replace `Either` with `InterfaceStatement`
                    Identifier => self.partial_static_field()?.either(Stmt::StaticRequirement, Stmt::StaticDefinition),
                    unexpected => return Err(Error::UnexpectedToken(TokenType::Identifier, unexpected))
                },
                unexpected => return Err(Error::UnexpectedToken(TokenType::Identifier, unexpected))
            };

            statements.push(statement);
        }


        return Ok(ast::InterfaceBody { statements });
    }
    
    fn class_body(&mut self) -> Result<ast::ClassBody<'a>> {
        self.consume(LeftBrace)?;

        let mut statements = Vec::new();
        while !self.is_next(RightBrace) {
            use ast::ClassStatement as Stmt;

            let statement = match self.next().token_type {
                Identifier => Stmt::Field(self.field()?),
                Type => Stmt::TypeDefinition(self.type_definition()?),
                Fn => Stmt::Method(self.function()?),
                Static => match self.next().token_type {
                    Fn => Stmt::StaticMethod(self.function()?),
                    Identifier => Stmt::StaticField(self.static_field()?),
                    unexpected => return Err(Error::UnexpectedToken(Identifier, unexpected))
                },
                unexpected => return Err(Error::UnexpectedToken(Identifier, unexpected))
            };

            statements.push(statement);
        }


        return Ok(ast::ClassBody { statements });
    }
    
    /// Assumes `Identifier` was consumed
    fn field(&mut self) -> Result<ast::Field<'a>> {
        let name = ast::Identifier(self.previous());

        self.consume(Colon)?;

        let item_type = self.type_literal()?;

        return Ok(ast::Field {
            name,
            item_type
        });
    }
    
    /// Assumes `Identifier` is already consumes
    fn partial_static_field(&mut self) -> Result<Either<ast::PartialStaticField<'a>, ast::StaticField<'a>>> {
        let name = ast::Identifier(self.previous());

        self.consume(Colon)?;
        let item_type = self.type_literal()?;

        if self.is_next(Equals) {
            let value = self.expression()?;
            
            return Ok(Right(ast::StaticField {
                name,
                item_type,
                value
            }));
        }

        return Ok(Left(ast::PartialStaticField {
            name,
            item_type
        }));
    }
    
    fn type_definition(&mut self) -> Result<ast::TypeDefinition<'a>> {
        let name = self.identifier()?;
        let generics = self.generic_params()?;
        let type_value = self.type_literal()?;

        return Ok (ast::TypeDefinition {
            name,
            generics,
            type_value
        });
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
            return Err(Error::UnexpectedInContext("use statement", self.peek().token_type));
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
                return Err(Error::UnexpectedInContext("use block", self.peek().token_type));
            }
            
            if !self.is_next(Comma) {
                break;
            }
        }
        
        return Ok(block);
    }
    
    fn forever(&mut self) -> Result<ast::Block<'a>> {
        self.consume(Forever)?;
        return self.function_body();
    }
    
    fn while_statement(&mut self) -> Result<ast::WhileStatement<'a>> {
        self.consume(While)?;

        let condition = self.expression()?;

        let body = self.function_body()?;

        let mut else_branch = None;
        if self.is_next(Else) {
            else_branch = Some(self.function_body()?);
        }
        
        return Ok(ast::WhileStatement {
            condition,
            body,
            else_branch
        });
    }
    
    fn for_statement(&mut self) -> Result<ast::ForStatement<'a>> {
        self.consume(For)?;

        if let Ok(loop_variable) = self.match_clause() {
            if self.is_next(In) {
                let iterator = self.expression()?;
                let body = self.function_body()?;

                return Ok(ast::ForStatement {
                    loop_variable: Some(loop_variable),
                    iterator,
                    body
                });
            } else {
                let body = self.function_body()?;

                return Ok(ast::ForStatement {
                    loop_variable: None,
                    iterator: loop_variable.as_literal()
                        .map(ast::Expression::Literal)
                        .ok_or_else(|| Error::Other("Invalid literal in for loop.".to_owned()))?,
                    body
                });
            }
        }

        let iterator = self.expression()?;
        let body = self.function_body()?;

        return Ok(ast::ForStatement {
            loop_variable: None,
            iterator,
            body
        });
    }
    
    /// Assumes the head of the path is already consumed
    fn type_path(&mut self, mut path: Vec<ast::Identifier<'a>>) -> Result<Vec<ast::Identifier<'a>>> {
        while self.is_next(Dot) {
            let name = self.identifier()?;
            path.push(name);
        }

        return Ok(path);
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
    
    fn equality(&mut self) -> Result<ast::Expression<'a>>  {
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

        while let Some(operator) = self.matches(&[Minus, Plus]) {
            let right = self.factor()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }

        return Ok(expr);
    }
    
    fn factor(&mut self) -> Result<ast::Expression<'a>> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.matches(&[Slash, Star]) {
            let right = self.unary()?;
            expr = ast::Expression::Binary(operator, Box::new(expr), Box::new(right));
        }
        
        return Ok(expr);
    }
    
    fn unary(&mut self) -> Result<ast::Expression<'a>> {
        if let Some(operator) = self.matches(&[Not, Minus]) {
            let right = self.unary()?;
            return Ok(ast::Expression::Unary(operator, Box::new(right)));
        }
        
        return self.function_call();
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
                },
                '"' => {
                    pieces.push(Literal(&lexeme[start..i]));
                },
                _ => {}
            }
            start = i;
        }

        return Ok(ast::Literal::String(ast::InterpolatedString{
            token: string,
            pieces
        }));
    }
    
    fn interface(&mut self) -> Result<ast::Interface<'a>> {
        let name = self.identifier()?;

        let generics = self.generic_params()?;
        
        let mut parent = None;
        if self.is_next(Of) {
            parent = Some(self.named_type()?);
        }
        
        let body = self.interface_body()?;

        return Ok(ast::Interface {
            name,
            generics,
            parent,
            body
        })
    }
    
    fn mixin(&mut self) -> Result<ast::Mixin<'a>> {
        Ok(ast::Mixin {
            name: self.identifier()?,
            generics: self.generic_params()?,
            body: self.class_body()?
        })
    }
    
    fn extend_with(&mut self) -> Result<ast::ExtendWith<'a>> {
        let extend = self.type_literal()?;

        let mut with = None;
        if self.is_next(With) {
            with = Some(self.named_type()?);
        }
        
        let body = self.class_body()?;

        return Ok(ast::ExtendWith{
            extend,
            with,
            body
        })
    }
    
    fn enum_statement(&mut self) -> Result<ast::Enum<'a>> {
        let name = self.identifier()?;

        let generics = self.generic_params()?;

        let mut body = Vec::with_capacity(2);
        self.consume(LeftBrace)?;

        loop {
            use ast::EnumValue as Value;
            
            let discriminant = self.identifier()?;
            let value = match self.peek().token_type {
                Equals => {
                    self.next();
                    if let Ok(constant) = self.identifier() {
                        Value::NamedConstant(constant)
                    } else {
                        let literal = self.literal()?;
                        Value::Value(literal)
                    }
                },
                LeftParen => {
                    self.next();
                    Value::Tuple(self.tuple_type()?)
                },
                LeftBrace => {
                    self.next();
                    Value::Object(self.object_type()?)
                },
                _ => Value::None
            };

            body.push((discriminant, value));

            if !self.is_next(Comma) {
                break;
            }
        }
        self.consume(RightBrace);
        
        return Ok(ast::Enum{
            name,
            generics,
            body
        });
    }
    
    fn constant(&mut self) -> Result<ast::ConstAssignment<'a>> {
        let name = self.identifier()?;

        self.consume(Colon);
        let item_type = self.type_literal()?;

        self.consume(Equals);
        let value = self.expression()?;
        
        return Ok(ast::ConstAssignment {
            name,
            item_type,
            value
        });
    }
    
    fn match_clause(&mut self) -> Result<ast::MatchClause<'a>> {
        // Try to consume identifier as a type, since a valid identifier is a valid type
        let mut item_type = None;

        let name = if let Ok(type_or_name) = self.type_literal() {
            if let ast::Type::Named { ref path, ref generics } = type_or_name {
                if generics.len() == 0 && path.len() == 1 {
                    Some(path[0].clone())
                } else {
                    item_type = Some(type_or_name);
                    None
                }
            } else {
                return Err(Error::InvalidMatchClause)
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
        
        if item_type.is_none() {
            
        }

        let guard_clause = self.if_condition().ok();
        
        return Ok(ast::MatchClause {
            name,
            destructure,
            item_type,
            guard_clause
        })
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
            let not_token = Token{
                token_type: TokenType::Not,
                lexeme
            };
            condition = ast::Expression::Unary(not_token, Box::new(condition));
        }
        
        return Ok(condition);
    }
    
    fn break_statement(&mut self) -> Result<ast::BreakStatement<'a>> {
        self.consume(Break)?;
        
        let mut label = None;
        if self.is_next(Colon) {
            label = Some(self.identifier()?);
        }
        
        let value = self.expression().ok();

        return Ok(ast::BreakStatement {
            label,
            value
        });
    }
    
    fn continue_statement(&mut self) -> Result<Option<ast::Identifier<'a>>> {
        self.consume(Continue)?;

        let mut label = None;
        if self.is_next(Colon) {
            label = Some(self.identifier()?);
        }
        
        return Ok(label);
    }
    
    fn destructure(&mut self) -> Result<ast::Destructure<'a>> {
        todo!()
    }
    
    // Assumes `Static Identifier` is already consumed
    fn static_field(&mut self) -> Result<ast::StaticField<'a>> {
        let name = ast::Identifier(self.previous());

        self.consume(Colon)?;
        let item_type = self.type_literal()?;
        
        self.consume(Equals)?;
        let value = self.expression()?;

        return Ok(ast::StaticField {
            name,
            item_type,
            value
        });
    }
    
    /// Consumes a comma separated list of type literals and pushes them onto the provided vec of types
    fn type_list(&mut self, types: &mut Vec<ast::Type<'a>>) -> Result<()> {
        loop {
            let item_type = self.parse_type()?;
            types.push(item_type);

            if !self.is_next(Comma) {
                return Ok(());
            }
        }
    }
    
    /// Assumes "(" is already consumed
    fn tuple_type(&mut self) -> Result<Vec<ast::Type<'a>>> {
        let mut types = Vec::new();
        if !self.is_next(RightParen) {
            self.type_list(&mut types)?;
        }

        self.consume(RightParen)?;
        return Ok(types);
    }
    
    /// Parses an object type literal.
    /// Assumes "{" is already consumed
    fn object_type(&mut self) -> Result<Vec<(ast::Identifier<'a>, ast::Type<'a>)>> {
        let mut fields = Vec::new();
        
        loop {
            let name = self.identifier()?;
            self.consume(Colon)?;
            let field_type = self.parse_type()?;
            
            fields.push((name, field_type));
            
            if !self.is_next(Comma) {
                break;
            }
        }
        self.consume(RightBrace)?;

        return Ok(fields);
    }
    
    fn array_type(&mut self) -> Result<Vec<(ast::Type<'a>, Option<usize>)>> {
        todo!()
    }
    
    fn function_type(&mut self) -> Result<ast::FunctionType<'a>> {
        self.consume(LeftParen)?;
        let parameters = self.tuple_type()?;
        
        let return_type = if self.is_next(Colon) {
            Some(Box::new(
                self.parse_type()?
            ))
        } else {
            None
        };

        return Ok(ast::FunctionType {
            parameters,
            return_type
        });
    }
}

fn get_char(string: &str, index: usize) -> Option<char> {
    string.get(index..(index + 1)).and_then(|s| s.chars().next())
}

fn base_64(c: char, _: u32) -> Option<u32> {
    Some(match c {
        'A'..='Z' => (c as u32) - ('A' as u32),
        'a'..='z' => 26 + (c as u32) - ('a' as u32),
        '0'..='9' => 52 + (c as u32) - ('0' as u32),
        '+' => 62,
        '/' => 63,
        _ => return None
    })
}

pub enum Warning {
    Other(std::string::String)
}

#[derive(Clone)]
pub enum Error {
    NumberTooLarge,
    UnexpectedToken(TokenType, TokenType),
    UnexpectedInContext(&'static str, TokenType),
    InvalidLeftValue(TokenType),
    InvalidMatchClause,
    Other(std::string::String)
}

type Result<T> = std::result::Result<T, Error>;