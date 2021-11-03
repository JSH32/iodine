use self::{
    error::{Error, Result},
    token::{Float, Integer, Radix, Token},
};
use std::str::FromStr;

pub mod error;
pub mod token;

pub struct Lexer<'a> {
    input: &'a str,
    cur_position: usize,
    read_position: usize,
    ch: char,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            cur_position: 0,
            read_position: 0,
            ch: 0 as char,
        };

        // Advance once to ready the Lexer
        l.read_char();
        l
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];

        // Consume tokens from the stream until Eof.
        loop {
            match self.next_token()? {
                t @ Token::Eof => {
                    tokens.push(t);
                    return Ok(tokens);
                }
                t => {
                    tokens.push(t);
                }
            }
        }
    }

    /// Advances the lexer once and produces a [`Token`]
    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespace();

        let t = match self.ch {
            '=' => {
                // Check if this is an equal operator or a set operator
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '!' => {
                // Is this '!=' or just '!'?
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Exclaim
                }
            }
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '%' => Token::Percent,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '&' => Token::Ampersand,
            ',' => Token::Comma,
            ':' => Token::Colon,
            ';' => Token::Semicolon,
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            '"' => self.read_string()?,
            '\u{0000}' => Token::Eof,

            _ => {
                if is_letter(self.ch) {
                    let ident = self.read_identifier();

                    // Determine if this identifier is actually a keyword and return that keyword
                    if let Some(key) = lookup_keyword(&ident) {
                        return Ok(key);
                    } else {
                        return Ok(Token::Identifier(ident));
                    }
                } else if is_number(self.ch) {
                    return Ok(self.read_number()?);
                } else {
                    // No known tokens for this character, return Illegal.
                    Token::Illegal(self.ch)
                }
            }
        };

        // Advance to the next character in preparation for the next call.
        self.read_char();
        Ok(t)
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            0 as char
        } else {
            // TODO(mdlayher): consider handling unicode?
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                ch
            } else {
                panic!("peeked out of range character")
            }
        }
    }

    /// Advance current position
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0 as char;
        } else {
            if let Some(ch) = self.input.chars().nth(self.read_position) {
                self.ch = ch;
            } else {
                panic!("read out of range character");
            }
        }

        self.cur_position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> String {
        let pos = self.cur_position;

        // Numbers okay in identifiers after first character.
        while is_letter(self.ch) || self.ch.is_numeric() {
            self.read_char();
        }

        self.input
            .chars()
            .skip(pos)
            .take(self.cur_position - pos)
            .collect()
    }

    fn read_number(&mut self) -> Result<Token> {
        let pos = self.cur_position;

        while (self.ch.is_ascii_alphanumeric() || self.ch == '.') && !self.ch.is_whitespace() {
            self.read_char();
        }

        let chars: Vec<char> = self
            .input
            .chars()
            .skip(pos)
            .take(self.cur_position - pos)
            .collect();

        if chars.contains(&'.') {
            Ok(Token::Float(Float::from(
                f64::from_str(&chars.iter().collect::<String>()).map_err(Error::IllegalFloat)?,
            )))
        } else {
            Ok(Token::Integer(parse_int(&chars)?))
        }
    }

    fn read_string(&mut self) -> Result<Token> {
        let pos = self.cur_position + 1;

        // Read characters until end quote or EOF.
        loop {
            self.read_char();

            match self.ch {
                '"' => break,
                '\u{0000}' => {
                    return Err(Error::UnexpectedEof);
                }
                _ => {}
            }
        }

        Ok(Token::String(
            self.input
                .chars()
                .skip(pos)
                .take(self.cur_position - pos)
                .collect(),
        ))
    }

    // Advance till rid of whitespace
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

fn lookup_keyword(str: &str) -> Option<Token> {
    Some(match str {
        "fn" => Token::Function,
        "let" => Token::Let,
        "const" => Token::Const,
        "true" => Token::Boolean(true),
        "false" => Token::Boolean(false),
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "set" => Token::Set,
        _ => return None,
    })
}

fn is_letter(c: char) -> bool {
    c.is_ascii_alphabetic() || c == '_'
}

fn is_number(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn parse_int(chars: &[char]) -> Result<Integer> {
    // If the numeric string is too short to contain a radix, assume base 10.
    if chars.len() < 2 {
        let raw: String = chars.iter().collect();
        return Ok(Integer {
            radix: Radix::Decimal,
            value: i64::from_str_radix(&raw, 10).map_err(Error::IllegalInteger)?,
        });
    }

    // Infer the radix and the number of prefix characters to skip when
    // parsing the numeric string.
    let (radix, skip) = match &chars[0..2] {
        // Binary
        ['0', 'b'] => (Radix::Binary, 2),
        // Hexadecimal
        ['0', 'x'] => (Radix::Hexadecimal, 2),
        // Octal
        ['0', 'o'] => (Radix::Octal, 2),
        // C-style octal
        ['0', '0'..='9'] => (Radix::Octal, 1),
        // Unknown
        ['0', r] => {
            return Err(Error::IllegalIntegerRadix(*r));
        }
        // Decimal
        _ => (Radix::Decimal, 0),
    };

    let raw: String = chars.iter().skip(skip).collect();
    let base = match radix {
        Radix::Binary => 2,
        Radix::Decimal => 10,
        Radix::Hexadecimal => 16,
        Radix::Octal => 8,
    };

    Ok(Integer {
        radix,
        value: i64::from_str_radix(&raw, base).map_err(Error::IllegalInteger)?,
    })
}
