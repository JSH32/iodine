use std::fmt;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Token {
    // Control tokens.
    Illegal(char),
    Eof,

    // Identifiers and literals.
    Identifier(String),
    Integer(Integer),
    Float(Float),
    String(String),
    Boolean(bool),

    // Operators
    Assign,
    Plus,
    Minus,
    Exclaim,
    Asterisk,
    Slash,
    Percent,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    Ampersand,

    // Delimiters.
    Comma,
    Colon,
    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,

    // Keywords.
    Function,
    Let,
    Const,
    If,
    Else,
    Return,
    Set,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Radix {
    Binary,
    Decimal,
    Hexadecimal,
    Octal,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Integer {
    pub radix: Radix,
    pub value: i64,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Float(pub u64);

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Illegal(c) => write!(f, "illegal({})", c),
            Token::Eof => write!(f, "EOF"),

            Token::Identifier(s) => write!(f, "identifier({})", s),
            Token::Integer(i) => i.fmt(f),
            Token::Float(fl) => fl.fmt(f),
            Token::String(s) => s.fmt(f),
            Token::Boolean(bool) => write!(f, "{}", bool),

            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Exclaim => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::LessThan => write!(f, "<"),
            Token::GreaterThan => write!(f, ">"),
            Token::Ampersand => write!(f, "&"),

            Token::Comma => write!(f, ","),
            Token::Colon => write!(f, ":"),
            Token::Semicolon => write!(f, ";"),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::LeftBracket => write!(f, "["),
            Token::RightBracket => write!(f, "]"),

            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::Const => write!(f, "const"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
            Token::Set => write!(f, "set"),
        }
    }
}

impl fmt::Display for Integer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.radix {
            Radix::Binary => write!(f, "0b{:b}", self.value),
            Radix::Decimal => write!(f, "{}", self.value),
            Radix::Hexadecimal => write!(f, "0x{:x}", self.value),
            Radix::Octal => write!(f, "0o{:o}", self.value),
        }
    }
}

impl Into<f64> for Float {
    fn into(self) -> f64 {
        f64::from_bits(self.0)
    }
}

impl From<f64> for Float {
    fn from(f: f64) -> Self {
        Self(f64::to_bits(f))
    }
}

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let float: f64 = self.clone().into();
        write!(f, "{}", float)
    }
}