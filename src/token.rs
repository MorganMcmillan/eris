use std::ops::Range;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    // Single character tokens
    Comma,
    Dollar,
    At,
    Hash,
    Question,
    Bang,
    Tilde,
    // Matching pairs of tokens
    Dot,
    DotDot,
    DotDotDot,
    DotDotEquals,
    Colon,
    ColonColon,
    ColonGreater,
    ColonGreaterGreater,
    LessLess,
    GreaterGreater, // Note: not scanned directly, but constructed by the parser during parsing
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
    Do,
    Else,
    Enum,
    Extend,
    False,
    Fn,
    For,
    Loop,
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
    Error,
    Eof,
}

impl TokenType {
    pub fn is_assignment_operator(&self) -> bool {
        use TokenType::*;
        matches!(self,
            Equals
            | PlusEquals
            | MinusEquals
            | StarEquals
            | SlashEquals
            | PercentEquals
            | PipeEquals
            | AmpersandEquals
            | CaratEquals
            | PlusPlusEquals
            | StarStarEquals
        )
    }

    pub fn infix_binding_power(&self) -> Option<(u8, u8)> {
        use TokenType::*;
        let bp = match *self {
            Or => (1, 2),
            And => (3, 4),
            Is => (5, 6),
            Less | LessEquals | Greater | GreaterEquals => (6, 7),
            EqualsEquals | BangEquals => (8, 9),
            Pipe => (10, 11),
            Carat => (12, 13),
            Ampersand => (14, 15),
            LessLess | GreaterGreater => (16, 17),
            PlusPlus => (19, 18),
            StarStar => (21, 20),
            In => (22, 23),
            Plus | Minus => (24, 25),
            Star | Slash | Percent => (26, 27),
            As => (28, 29),
            // After question
            ColonGreater | ColonGreaterGreater => (32, 33),
            _ => return None
        };
        return Some(bp)
    }

    pub fn prefix_binding_power(&self) -> Option<((), u8)> {
        use TokenType::*;
        // After As
        match *self {
            Not | Minus | Tilde | Ampersand => Some(((), 30)),
            _ => None
        }
    }
    
    pub fn postfix_binding_power(&self) -> Option<(u8, ())> {
        use TokenType::*;
        match *self {
            // after unary
            Question | Bang => Some((31, ())),
            // After pipe operators
            Dot | LeftParen | LeftBracket => Some((34, ())),
            _ => None
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str) -> Self {
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
}