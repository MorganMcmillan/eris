use logos::Logos;

#[derive(Logos, Clone, Copy, Debug, PartialEq, Eq)]
#[logos(skip r" \t\r\n\f")]
pub enum Token<'a> {
    // Single character tokens
    #[token(",")]
    Comma,
    #[token("$")]
    Dollar,
    #[token("@")]
    At,
    #[token("#")]
    Hash,
    #[token("?")]
    Question,
    #[token("!")]
    Bang,
    #[token("~")]
    Tilde,
    // Matching pairs of tokens
    #[token(".")]
    Dot,
    #[token("..")]
    DotDot,
    #[token("...")]
    DotDotDot,
    #[token("..=")]
    DotDotEquals,
    #[token(":")]
    Colon,
    #[token("::")]
    ColonColon,
    #[token(":>")]
    ColonGreater,
    #[token(":>>")]
    ColonGreaterGreater,
    #[token("<<")]
    LessLess,
    GreaterGreater, // Note: not scanned directly, but constructed by the parser during parsing
    // Balanced pairs
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token("\x7b")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    // Operators with assignment variants
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEquals,
    #[token("++")]
    PlusPlus,
    #[token("++=")]
    PlusPlusEquals,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEquals,
    #[token("*")]
    Star,
    #[token("*=")]
    StarEquals,
    #[token("**")]
    StarStar,
    #[token("**=")]
    StarStarEquals,
    #[token("/")]
    Slash,
    #[token("/=")]
    SlashEquals,
    #[token("%")]
    Percent,
    #[token("%=")]
    PercentEquals,
    #[token("=")]
    Equals,
    #[token("==")]
    EqualsEquals,
    #[token("!=")]
    BangEquals,
    #[token("<")]
    Less,
    #[token("<=")]
    LessEquals,
    #[token(">")]
    Greater,
    #[token(">=")]
    GreaterEquals,
    #[token("|")]
    Pipe,
    #[token("|=")]
    PipeEquals,
    #[token("&")]
    Ampersand,
    #[token("&=")]
    AmpersandEquals,
    #[token("^")]
    Carat,
    #[token("^=")]
    CaratEquals,
    // Tokens than may repeat
    // Keywords
    #[token("abstract")]
    Abstract,
    #[token("and")]
    And,
    #[token("as")]
    As,
    #[token("break")]
    Break,
    #[token("class")]
    Class,
    #[token("const")]
    Const,
    #[token("continue")]
    Continue,
    #[token("do")]
    Do,
    #[token("else")]
    Else,
    #[token("enum")]
    Enum,
    #[token("extend")]
    Extend,
    #[token("false")]
    False,
    #[token("fn")]
    Fn,
    #[token("for")]
    For,
    #[token("loop")]
    Loop,
    #[token("if")]
    If,
    #[token("in")]
    In,
    #[token("interface")]
    Interface,
    #[token("is")]
    Is,
    #[token("let")]
    Let,
    #[token("macro")]
    Macro,
    #[token("match")]
    Match,
    #[token("mixin")]
    Mixin,
    #[token("never")]
    Never,
    #[token("nil")]
    Nil,
    #[token("not")]
    Not,
    #[token("of")]
    Of,
    #[token("or")]
    Or,
    #[token("return")]
    Return,
    #[token("self")]
    LowercaseSelf,
    #[token("Self")]
    UppercaseSelf,
    #[token("static")]
    Static,
    #[token("super")]
    LowercaseSuper,
    #[token("Super")]
    UppercaseSuper,
    #[token("true")]
    True,
    #[token("type")]
    Type,
    #[token("unless")]
    Unless,
    #[token("until")]
    Until,
    #[token("use")]
    Use,
    #[token("while")]
    While,
    #[token("with")]
    With,
    // Literals
    #[regex(r"([a-zA-Z_]\w*)", |lex| lex.slice())]
    Identifier(&'a str),
    String,
    // TODO: Implement char and string parsing
    #[regex(r"'(\\|.)'")]
    Char,
    // Number literals
    Base64,
    Base36,
    Base32,
    #[regex(r"0h[0-9a-fA-F_]+")]
    Hexadecimal,
    #[regex(r"\d[\d_]*")]
    #[regex(r"0d[\d_]+")]
    Decimal,
    #[regex(r"0o[0-7_]+")]
    Octal,
    #[regex(r"0q[0-3_]+")]
    Quadal,
    #[regex(r"0b[01_]+")]
    Binary,
    // Other
    #[token("->")]
    MinusArrow,
    Error,
    Eof,
}

impl Token {
    pub fn is_assignment_operator(&self) -> bool {
        use Token::*;
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
        use Token::*;
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
        use Token::*;
        // After As
        match *self {
            Not | Minus | Tilde | Ampersand => Some(((), 30)),
            _ => None
        }
    }
    
    pub fn postfix_binding_power(&self) -> Option<(u8, ())> {
        use Token::*;
        match *self {
            // after unary
            Question | Bang => Some((31, ())),
            // After pipe operators
            Dot | LeftParen | LeftBracket => Some((34, ())),
            _ => None
        }
    }
}