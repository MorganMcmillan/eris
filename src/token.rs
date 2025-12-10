use std::ops::Range;

use crate::scanner::Scanner;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single character tokens
    Comma,
    Dollar,
    At,
    Hash,
    Question,
    // Matching pairs of tokens
    Dot,
    DotDot,
    DotDotDot,
    DotDotEquals,
    Colon,
    ColonColon,
    ColonArrow,
    // Potential multi character tokens
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    // Operators with assignment variants
    Plus,
    PlusEquals,
    PlusPlus,
    PlusPlusEquals,
    Minus,
    MinusEquals,
    Star,
    StarEquals,
    StarStar,
    StarStarEquals,
    Slash,
    SlashEquals,
    Percent,
    PercentEquals,
    Equals,
    EqualsEquals,
    BangEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Pipe,
    PipeEquals,
    Ampersand,
    AmpersandEquals,
    Carat,
    CaratEquals,
    // Tokens than may repeat
    // Keywords
    Abstract,
    And,
    As,
    Break,
    Class,
    Const,
    Continue,
    Else,
    Elseif,
    Enum,
    Extend,
    False,
    Fn,
    For,
    Forever,
    If,
    In,
    Interface,
    Is,
    Let,
    Macro,
    Match,
    Mixin,
    Never,
    Nil,
    Not,
    Of,
    Or,
    Return,
    LowercaseSelf,
    UppercaseSelf,
    Static,
    LowercaseSuper,
    UppercaseSuper,
    True,
    Type,
    Unless,
    Until,
    Use,
    While,
    With,
    // Literals
    Identifier,
    String,
    Char,
    // Number literals
    Base64,
    Base36,
    Base32,
    Hexadecimal,
    Decimal,
    Octal,
    Quadal,
    Binary,
    // Other
    MinusArrow,
    Le,
    Error,
    Eof,
}

fn is_base_64(c: char) -> bool {
    is_alphanumeric(c) || c == '+' || c == '/'
}

fn is_alphanumeric(c: char) -> bool {
    c.is_alphanumeric() || c == '_'
}

fn is_base_32(c: char) -> bool {
    is_decimal(c) || matches!(c, 'a'..='w' | 'A'..='W')
}

fn is_hex(c: char) -> bool {
    c.is_ascii_hexdigit() || c == '_'
}

fn is_decimal(c: char) -> bool {
    matches!(c, '0'..='9' | '_')
}

fn is_octal(c: char) -> bool {
    matches!(c, '0'..='7' | '_')
}

fn is_quadal(c: char) -> bool {
    matches!(c, '0'..='3' | '_')
}

fn is_binary(c: char) -> bool {
    matches!(c, '0' | '1' | '_')
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
}

// TODO: replace static functions with scanner methods

impl<'a> Token<'a> {
    fn new(token_type: TokenType, lexeme: &'a str) -> Self {
        Self {
            token_type,
            lexeme
        }
    }

    pub fn span(&self, input: &'a str) -> Range<usize> {
        let starting_address = input.as_ptr() as usize;
        let start = self.lexeme.as_ptr() as usize - starting_address;
        let end = start + self.lexeme.len();
        return start..end;
    }

    // Check if this token sits right next to another token (meaning there's no whitespace separating them)
    pub fn is_next_to(&self, other: &Token) -> bool {
        let this_address = self.lexeme.as_ptr() as usize + self.lexeme.len();
        return this_address == other.lexeme.as_ptr() as usize;
    }

    fn keyword(scanner: &mut Scanner<'a>, keywords: &[(&str, TokenType)]) -> TokenType {
        // TODO: replace with trie
        let word = Token::identifier(scanner);
        for (keyword, token) in keywords {
            if &keyword[1..] == word {
                return *token;
            }
        }
        return TokenType::Identifier;
    }

    fn identifier(scanner: &mut Scanner<'a>) -> &'a str {
        scanner.take_while(&is_alphanumeric)
    }

    fn scan_token(scanner: &mut Scanner<'a>) -> Option<Self> {
        scanner.save_position();

        use TokenType::*;
        let token_type = match scanner.next()? {
            ',' => Comma,
            '.' => match scanner.peek() {
                Some('.') => {
                    scanner.next();
                    match scanner.peek() {
                        Some('.') => {
                            scanner.next();
                            DotDotDot
                        },
                        Some('=') => {
                            scanner.next();
                            DotDotEquals
                        },
                        _ => DotDot
                    }
                },
                _ => Dot
            }
            ,
            '$' => Dollar,
            '?' => Question,
            // TODO: match both macro identifiers and hash-strings
            '@' => At,
            '#' => Hash,
            '(' => LeftParen,
            ')' => RightParen,
            '[' => LeftBracket,
            ']' => RightBracket,
            '{' => LeftBrace,
            '}' => RightBrace,
            ':' => match scanner.peek() {
                Some(':') => {
                    scanner.next();
                    ColonColon
                },
                Some('>') => {
                    scanner.next();
                    ColonArrow
                },
                _ => Colon
            },
            '+' => match scanner.peek() {
                Some('=') => {
                    scanner.next();
                    PlusEquals
                },
                Some('+') => {
                    scanner.next();
                    if scanner.peek() == Some('=') {
                        scanner.next();
                        PlusPlusEquals
                    } else {
                        PlusPlus
                    }
                },
                _ => Plus
            },
            '-' => match scanner.peek() {
                Some('=') => {
                    scanner.next();
                    MinusEquals
                },
                Some('>') => {
                    scanner.next();
                    MinusArrow
                }
                _ => Minus,
            },
            '*' => match scanner.peek() {
                Some('=') => {
                    scanner.next();
                    StarEquals
                },
                Some('*') => {
                    scanner.next();
                    match scanner.peek() {
                        Some('=') => {
                            scanner.next();
                            StarStarEquals
                        },
                        _ => StarStar
                    }
                },
                _ => Star
            },
            '/' => match scanner.peek() {
                Some('=') => {
                    scanner.next();
                    SlashEquals    
                },
                // TODO: maybe allow parsing comments (mainly "///" doc comments) and handle skipping comments in the parser?
                Some('/') => {
                    scanner.take_while(&|c| c != '\n');
                    return Token::scan_token(scanner);
                }
                _ => Slash
            },
            '%' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    PercentEquals
                } else {
                    Percent
                }
            },
            '&' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    AmpersandEquals
                } else {
                    Ampersand
                }
            },
            '|' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    PipeEquals
                } else {
                    Pipe
                }
            },
            '^' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    CaratEquals
                } else {
                    Carat
                }
            },
            '=' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    EqualsEquals
                } else {
                    Equals
                }
            },
            '<' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    LessEquals
                } else {
                    Less
                }
            },
            '>' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    GreaterEquals
                } else {
                    Greater
                }
            },
            '!' => {
                if scanner.peek() == Some('=') {
                    scanner.next();
                    BangEquals
                } else {
                    Error
                }
            },
            'a' => Token::keyword(scanner, &[("abstract", Abstract), ("and", And), ("as", As)]),
            'b' => Token::keyword(scanner, &[("break", Break)]),
            'c' => Token::keyword(scanner, &[("class", Class), ("const", Const), ("continue", Continue)]),
            'e' => Token::keyword(scanner, &[("else", Else), ("elseif", Elseif), ("enum", Enum), ("extend", Extend)]),
            'f' => Token::keyword(scanner, &[("false", False), ("fn", Fn), ("for", For), ("forever", Forever)]),
            'i' => Token::keyword(scanner, &[("if", If), ("in", In), ("interface", Interface), ("is", Is)]),
            'l' => Token::keyword(scanner, &[("let", Let)]),
            'm' => Token::keyword(scanner, &[("macro", Macro), ("match", Match), ("mixin", Mixin)]),
            'n' => Token::keyword(scanner, &[("never", Never), ("nil", Nil), ("not", Not)]),
            'o' => Token::keyword(scanner, &[("of", Of), ("or", Or)]),
            'r' => Token::keyword(scanner, &[("return", Return)]),
            's' => Token::keyword(scanner, &[("self", LowercaseSelf), ("static", Static), ("super", LowercaseSuper)]),
            'S' => Token::keyword(scanner, &[("Self", UppercaseSelf), ("Super", UppercaseSuper)]),
            't' => Token::keyword(scanner, &[("true", True), ("type", Type)]),
            'u' => Token::keyword(scanner, &[("unless", Unless), ("until", Until), ("use", Use)]),
            'w' => Token::keyword(scanner, &[("while", While), ("with", With)]),
            '0' => Token::number_with_base(scanner).unwrap_or(Error),
            '1'..='9' => Token::number(scanner),
            '"' => Token::string(scanner).unwrap_or(Error),
            '\'' => Token::char(scanner).unwrap_or(Error),
            ws if ws.is_whitespace() => {
                scanner.take_while(&char::is_whitespace);
                return Token::scan_token(scanner);
            },
            _ => Error
        };

        return Some(Token::new(token_type, scanner.lexeme()));
    }

    pub fn from_input(input: &'a str) -> Vec<Self> {
        let mut tokens = Vec::with_capacity(input.len());
        let mut scanner = Scanner::new(input);

        while let Some(token) = Self::scan_token(&mut scanner) {
            tokens.push(token);
        }

        tokens.push(Token::new(TokenType::Eof, scanner.rest()));

        return tokens;
    }
    
    /// Matches a number with a given base.
    /// Unlike most other programming languages, Eris allows a wider range of base specifiers.
    /// Allowed bases include:
    /// "0s" base-64
    /// "0a" base 36 alphanumeric
    /// "0t" base-32
    /// "0x" base-16 hexadecimal
    /// "0d" base-10 decimal
    /// "0o" base-8 octal
    /// "0q" base-4 quadal
    /// "0b" base-2 binary
    fn number_with_base(scanner: &mut Scanner<'a>) -> Option<TokenType> {
        use TokenType::*;
        if let Some(format) = scanner.peek() {
            let (predicate, token): (fn (char) -> bool, TokenType) = match format {
                's' => (is_base_64, Base64),
                'a' => (is_alphanumeric, Base36),
                't' => (is_base_32, Base32),
                'x' => (is_hex, Hexadecimal),
                'd' => (is_decimal, Decimal),
                'o' => (is_octal, Octal),
                'q' => (is_quadal, Quadal),
                'b' => (is_binary, Binary),
                _ => return None
            };
            // Prove that number is not empty
            scanner.next();
            let number = scanner.take_while(&predicate);

            if number.is_empty() {
                return None;
            };

            return Some(token);
        }
        return Some(Decimal);
    }
    
    fn number(scanner: &mut Scanner<'a>) -> TokenType {
        use TokenType::*;
        
        scanner.take_while(&is_decimal);
        if !scanner.peek().map(char::is_whitespace).unwrap_or(true) {
            return Error;
        }
        return Decimal
    }
    
    fn string(scanner: &mut Scanner<'a>) -> Option<TokenType> {
        // TODO: strings preceded by multiple '#' symbols may need to know how many there are so they can tell when they end
        let mut c: char = scanner.next()?;
        while c != '"' {
            if c == '\\' {
                // Skip possible quote character
                scanner.next();
            }
            c = scanner.next()?;
        }
        return Some(TokenType::String);
    }
    
    fn char(scanner: &mut Scanner<'a>) -> Option<TokenType> {
        let mut c: char = scanner.next()?;
        if c == '\\' {
            c = scanner.next()?;
        }

        if c == '\'' {
            return Some(TokenType::Char);
        } else {
            return None;
        }
    }
}