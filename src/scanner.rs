use crate::token::{Token, TokenType};

pub struct Scanner<'a> {
    /// The input to the scanner.
    input: &'a str,
    /// The current position of the scanner, as the index of a single valid unicode character
    position: usize,
    /// The last saved position, used to backtrack when a token from a sequence fails
    last_position: usize,
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

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            position: 0,
            last_position: 0,
        }
    }

    pub fn peek(&self) -> Option<char> {
        let end = self.input.ceil_char_boundary(self.position + 1);
        return self.input.get(self.position..end).and_then(|c| c.chars().next());
    }

    pub fn next(&mut self) -> Option<char> {
        let start = self.position;
        let end = self.input.ceil_char_boundary(start + 1);
        // println!("Start: {start}, End: {end}");
        self.position = end;
        return self.input.get(start..end).and_then(|c| c.chars().next());
    }

    pub fn save_position(&mut self) {
        self.last_position = self.position;
    }

    pub fn lexeme(&self) -> &'a str {
        &self.input[self.last_position..self.position]
    }

    pub fn rest(&self) -> &'a str {
        &self.input[self.position..]
    }
    
    pub fn take_while(&mut self, predicate: &impl std::ops::Fn(char) -> bool) -> &'a str {
        let start = self.position;
        while self.peek().map(predicate).unwrap_or(false) {
            self.next();
        }
        return &self.input[start..self.position];
    }

    fn keyword(&mut self, keywords: &[(&str, TokenType)]) -> TokenType {
        // TODO: replace with trie
        let word = self.identifier();
        for (keyword, token) in keywords {
            if &keyword[1..] == word {
                return *token;
            }
        }
        return TokenType::Identifier;
    }

    fn identifier(&mut self) -> &'a str {
        self.take_while(&is_alphanumeric)
    }

    fn scan_token(&mut self) -> Option<Token<'a>> {
        self.save_position();

        use TokenType::*;
        let token_type = match self.next()? {
            ',' => Comma,
            '.' => match self.peek() {
                Some('.') => {
                    self.next();
                    match self.peek() {
                        Some('.') => {
                            self.next();
                            DotDotDot
                        },
                        Some('=') => {
                            self.next();
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
            '~' => Tilde,
            ':' => match self.peek() {
                Some(':') => {
                    self.next();
                    ColonColon
                },
                Some('>') => {
                    self.next();
                    if self.peek() == Some('>') {
                        self.next();
                        ColonGreaterGreater
                    } else {
                        ColonGreater
                    }
                },
                _ => Colon
            },
            '+' => match self.peek() {
                Some('=') => {
                    self.next();
                    PlusEquals
                },
                Some('+') => {
                    self.next();
                    if self.peek() == Some('=') {
                        self.next();
                        PlusPlusEquals
                    } else {
                        PlusPlus
                    }
                },
                _ => Plus
            },
            '-' => match self.peek() {
                Some('=') => {
                    self.next();
                    MinusEquals
                },
                Some('>') => {
                    self.next();
                    MinusArrow
                }
                _ => Minus,
            },
            '*' => match self.peek() {
                Some('=') => {
                    self.next();
                    StarEquals
                },
                Some('*') => {
                    self.next();
                    match self.peek() {
                        Some('=') => {
                            self.next();
                            StarStarEquals
                        },
                        _ => StarStar
                    }
                },
                _ => Star
            },
            '/' => match self.peek() {
                Some('=') => {
                    self.next();
                    SlashEquals    
                },
                // TODO: maybe allow parsing comments (mainly "///" doc comments) and handle skipping comments in the parser?
                Some('/') => {
                    self.take_while(&|c| c != '\n');
                    return self.scan_token();
                }
                _ => Slash
            },
            '%' => {
                if self.peek() == Some('=') {
                    self.next();
                    PercentEquals
                } else {
                    Percent
                }
            },
            '&' => {
                if self.peek() == Some('=') {
                    self.next();
                    AmpersandEquals
                } else {
                    Ampersand
                }
            },
            '|' => {
                if self.peek() == Some('=') {
                    self.next();
                    PipeEquals
                } else {
                    Pipe
                }
            },
            '^' => {
                if self.peek() == Some('=') {
                    self.next();
                    CaratEquals
                } else {
                    Carat
                }
            },
            '=' => {
                if self.peek() == Some('=') {
                    self.next();
                    EqualsEquals
                } else {
                    Equals
                }
            },
            '<' => {
                match self.peek() {
                    Some('=') => {
                        self.next();
                        LessEquals
                    },
                    Some('<') => {
                        self.next();
                        LessLess
                    },
                    _ => Less
                }
            },
            '>' => {
                if self.peek() == Some('=') {
                    self.next();
                    GreaterEquals
                } else {
                    Greater
                }
            },
            '!' => {
                if self.peek() == Some('=') {
                    self.next();
                    BangEquals
                } else {
                    Bang
                }
            },
            'a' => self.keyword( &[("abstract", Abstract), ("and", And), ("as", As)]),
            'b' => self.keyword( &[("break", Break)]),
            'c' => self.keyword( &[("class", Class), ("const", Const), ("continue", Continue)]),
            'd' => self.keyword( &[("do", Do)]),
            'e' => self.keyword( &[("else", Else), ("enum", Enum), ("extend", Extend)]),
            'f' => self.keyword( &[("false", False), ("fn", Fn), ("for", For)]),
            'i' => self.keyword( &[("if", If), ("in", In), ("interface", Interface), ("is", Is)]),
            'l' => self.keyword( &[("let", Let), ("loop", Loop)]),
            'm' => self.keyword( &[("macro", Macro), ("match", Match), ("mixin", Mixin)]),
            'n' => self.keyword( &[("never", Never), ("nil", Nil), ("not", Not)]),
            'o' => self.keyword( &[("of", Of), ("or", Or)]),
            'r' => self.keyword( &[("return", Return)]),
            's' => self.keyword( &[("self", LowercaseSelf), ("static", Static), ("super", LowercaseSuper)]),
            'S' => self.keyword( &[("Self", UppercaseSelf), ("Super", UppercaseSuper)]),
            't' => self.keyword( &[("true", True), ("type", Type)]),
            'u' => self.keyword( &[("unless", Unless), ("until", Until), ("use", Use)]),
            'w' => self.keyword( &[("while", While), ("with", With)]),
            '0' => self.number_with_base().unwrap_or(Error),
            '1'..='9' => self.number(),
            '"' => self.string().unwrap_or(Error),
            '\'' => self.char().unwrap_or(Error),
            ws if ws.is_whitespace() => {
                self.take_while(&char::is_whitespace);
                return self.scan_token();
            },
            _ => if self.identifier().len() > 0 {
                Identifier
            } else {
                return None;
            }
        };

        return Some(Token::new(token_type, self.lexeme()));
    }

    pub fn scan(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::with_capacity(self.input.len());

        while let Some(token) = self.scan_token() {
            tokens.push(token);
        }

        tokens.push(Token::new(TokenType::Eof, self.rest()));

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
    fn number_with_base(&mut self) -> Option<TokenType> {
        use TokenType::*;
        if let Some(format) = self.peek() {
            let (predicate, token): (fn (char) -> bool, TokenType) = match format {
                's' => (is_base_64, Base64),
                'a' => (is_alphanumeric, Base36),
                't' => (is_base_32, Base32),
                'x' => (is_hex, Hexadecimal),
                'd' => (is_decimal, Decimal),
                'o' => (is_octal, Octal),
                'q' => (is_quadal, Quadal),
                'b' => (is_binary, Binary),
                _ => return Some(Decimal)
            };
            // Prove that number is not empty
            self.next();
            let number = self.take_while(&predicate);

            if number.is_empty() {
                return None;
            };

            return Some(token);
        }
        return Some(Decimal);
    }
    
    fn number(&mut self) -> TokenType {
        use TokenType::*;
        
        self.take_while(&is_decimal);
        if !self.peek().map(char::is_whitespace).unwrap_or(true) {
            return Error;
        }
        return Decimal
    }
    
    fn string(&mut self) -> Option<TokenType> {
        // TODO: strings preceded by multiple '#' symbols may need to know how many there are so they can tell when they end
        let mut c: char = self.next()?;
        while c != '"' {
            if c == '\\' {
                // Skip possible quote character
                self.next();
            }
            c = self.next()?;
        }
        return Some(TokenType::String);
    }
    
    fn char(&mut self) -> Option<TokenType> {
        let mut c: char = self.next()?;
        if c == '\\' {
            c = self.next()?;
        }

        if c == '\'' {
            return Some(TokenType::Char);
        } else {
            return None;
        }
    }
}