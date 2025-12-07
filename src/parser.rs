use crate::{ast::{self, Expression}, token::{Token, TokenType::{self, *}}};

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

    fn matches(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.next();
                return true;
            }
        }
        return false;
    }
    
    fn consume(&mut self, expected: TokenType) -> Result<Token<'a>> {
        if self.check(expected) {
            Ok(self.next())
        } else {
            Err(self.error(format!("Expected {expected:?}, got {:?}.", self.previous())))
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
            Abstract => self.abstract_class(),
            Interface => self.inteface(),
            Mixin => self.mixin(),
            Extend => self.extend_with(),
            Enum => self.enum_statement(),
            Type => self.type_definition(),
            Const => self.constant(),
            // Use => self.use_something(),
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

        let parent = None;
        if self.matches(&[Of]) {
            parent = Some(self.named_type()?);
        }

        let mut with = Vec::with_capacity(2);
        if self.matches(&[With]) {

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

    fn generic_params(&mut self) -> Result<Vec<ast::Generic>> {
        let generics = Vec::new();
        if self.is_next(Less) {
            loop {
                // let constant_name: ast::Identifier<'a>;
                let name = self.identifier()?;

                let supertype = None;
                if self.is_next(Colon) {
                    supertype = Some(self.type_literal()?);
                }

                let default_type = None;
                if self.is_next(Equals) {
                    default_type = Some(self.type_literal()?);
                }

                generics.push(ast::Generic {
                    name,
                    supertype,
                    default_type
                });

                if !self.is_next(Comma) {
                    self.consume(Greater)?;
                    break;
                }
            }
        }

        return Ok(generics);
    }

    fn type_literal(&mut self) -> Result<ast::Type> {
        todo!()
    }
    
    fn named_type(&mut self) -> Result<ast::Type<'a>> {
        let name = self.identifier()?;

        let mut generics = Vec::new();
        if self.is_next(Less) {
            loop {
                generics.push(self.named_type()?);

                if !self.is_next(Comma) {
                    self.consume(Greater)?;
                    break;
                }
            }
        }

        return Ok(ast::Type::Named(name, generics));
    }

    // Currently uses recursive descent
    fn expression(&mut self) -> Result<ast::Expression<'a>> {
        self.assignment()
    }

    fn identifier(&mut self) -> Result<ast::Identifier<'a>> {
        self.consume(Identifier).map(ast::Identifier)
    }

    fn function_declaration(&mut self) -> Result<ast::FunctionDeclaration<'a>> {
        self.consume(Fn)?;
        let name = self.identifier()?;
        let generics = self.generic_params()?;
                
        let parameters = Vec::new();
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

        let return_type = None;
        if self.is_next(Colon) {
            return_type = Some(self.type_literal()?);
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


        let value = None;
        if self.is_next(Equals) {
            value = Some(self.expression()?);
        }
        
        return Ok(ast::LetStatement {
            clause,
            value
        })
    }

    fn expression_list(&mut self) -> Result<Vec<Expression<'a>>> {
        let mut expressions = Vec::new();

        loop {
            expressions.push(self.expression()?);
            
            if !self.is_next(Comma) {
                return Ok(expressions);
            }
        }
    }

    fn array(&mut self) -> Result<Vec<ast::Expression<'a>>> {
        let array = self.expression_list()?;
        self.consume(RightBrace)?;
        return Ok(array);
    }

    fn number(&mut self, number: Token<'a>) -> Result<ast::Literal<'a>> {
        type CharMapper = fn(char, u32) -> Option<u32>;

        let (mapping, radix): (&fn(char, u32) -> Option<u32>, usize) = match number.token_type {
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
            return ast::Literal::Integer(output as u32);
        }

        let current = 0;
        if get_char(lexeme) == '0' {
            current += 2;
        }

        return Ok(());
    }

    fn primary_expression(&mut self) -> Result<ast::Expression<'a>> {
        use ast::Expression::Literal;
        Ok(match self.next().token_type {
            Nil => Literal(ast::Literal::Nil),
            False => Literal(ast::Literal::False),
            True => Literal(ast::Literal::True),
            String => Literal(self.parse_string(self.previous().lexeme)?),
            LeftParen => {
                let tuple = self.expression_list()?;
                self.consume(RightParen);
                if tuple.len() == 1 {
                    // Return as parenthesized expression
                    tuple[0]
                } else {
                    Literal(ast::Literal::Tuple(tuple))
                }
            },
            LeftBracket => Literal(ast::Literal::Array(self.array()?)),
            LeftBrace => Literal(ast::Literal::Object(self.object()?)),
            number_type => Literal(self.number(self.previous())?)
        })
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
        let expr = self.primary_expression()?;

        loop {
            if self.is_next(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.is_next(Dot) {
                let name = self.identifier()?;
                expr = ast::Expression::Field(Box::new(expr), name);
            } else {
                break;
            }
        }
        
        return expr;
    }

    fn assignment(&mut self) -> Result<ast::Assignment<'a>> {
        let expr = self.function_call()?;
        self.consume(Equals)?;

    }

    fn function_statement(&mut self) -> Result<ast::FunctionStatement<'a>> {
        use ast::FunctionStatement as S;
        Ok(self.let_variable().map(ast::FunctionStatement::Let)
        .or_else(|_|
            Ok(S::Assignment(self.assignment()?))
        ).or_else(|_|
            Ok(S::Forever(self.forever()?))
        ).or_else(|_|
            Ok(S::While(self.while_statement()?))
        ).or_else(|_|
            Ok(S::For(self.for_statement()?))
        ).or_else(|_|
            Ok(S::Return(self.return_statement()?))
        ).or_else(|_|
            Ok(S::Break(self.break_statement()?))
        ).or_else(|_|
            Ok(S::Continue(self.continue_statement()?))
        ).or_else(|_|
            Ok(S::Expression(self.expression()?))
        )?)
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
    
    fn function(&mut self) -> Result<ast::FunctionDefinition<'a>> {
        let declaration = self.function_declaration()?;
        let body = self.function_body()?;
        
        return Ok(ast::FunctionDefinition {
            declaration,
            body
        });
    }

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
    UnexpectedToken(TokenType, TokenType),
    Other(std::string::String)
}

type Result<T> = std::result::Result<T, Error>;