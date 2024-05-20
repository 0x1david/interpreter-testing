#![allow(dead_code)]
use std::char;

struct Lexer<'a> {
    source: &'a str,
    tokens: Vec<TokenKind>,
    chars: std::iter::Peekable<std::str::Chars<'a>>,

    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn add_token(&mut self, token: TokenKind) {
        self.tokens.push(token)
    }

    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars
            .next()
            .expect("We are looking for EOF before advancing.")
    }

    fn collect_until_token(&self, c: &char) -> String {
        unimplemented!()
    }

    fn advance_until_token(&mut self, c: char) {
        while let Some(&ch) = self.peek() {
            self.advance();
            if ch == c {
                break;
            };
        }
    }

    fn scan_token(&mut self) {
        let char = self.advance();
        match char {
            '(' => self.add_token(TokenKind::LeftParen),
            ')' => self.add_token(TokenKind::RightParen),
            '{' => self.add_token(TokenKind::LeftBrace),
            '}' => self.add_token(TokenKind::RightBrace),
            ',' => self.add_token(TokenKind::Comma),
            '.' => self.add_token(TokenKind::Dot),
            '-' => self.add_token(TokenKind::Minus),
            '+' => self.add_token(TokenKind::Plus),
            ';' => self.add_token(TokenKind::Semicolon),
            '*' => self.add_token(TokenKind::Star),
            '!' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    self.add_token(TokenKind::BangEqual)
                } else {
                    self.add_token(TokenKind::Bang)
                }
            }
            '=' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    self.add_token(TokenKind::EqualEqual)
                } else {
                    self.add_token(TokenKind::Equal)
                }
            }
            '<' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    self.add_token(TokenKind::LessEqual)
                } else {
                    self.add_token(TokenKind::Less)
                }
            }
            '>' => {
                if self.peek() == Some(&'=') {
                    self.advance();
                    self.add_token(TokenKind::GreaterEqual)
                } else {
                    self.add_token(TokenKind::Greater)
                }
            }
            '/' => {
                if self.peek() == Some(&'/') {
                    self.advance_until_token('\n');
                    self.advance();
                    self.line += 1;
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => {
                // From here on the rest of match is currently incomplete
                // TODO: Complete
                self.add_token(TokenKind::Newline);
                self.line += 1;
            }
            '0'..='9' => {
                // This is incorrect just no time to implement yet
                self.add_token(TokenKind::Integer(char as i64));
            }
            '"' => self.add_token(TokenKind::DoubleQuote),
            '\'' => self.add_token(TokenKind::SingleQuote),
            c if c.is_alphabetic() => {
                self.add_token(TokenKind::Identifier("placeholder".to_string()))
            }
            _ => self.add_token(TokenKind::UnrecognizedToken),
        };
    }

    fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
    }
}

enum Keyword {
    And,
    Class,
    Else,
    False,
    True,
    For,
    Proc,
    If,
    Null,
    Or,
    Return,
    Super,
    This,
    Let,
    Const,
    While,
}

impl Keyword {
    fn from_word(word: &str) -> Option<Self> {
        match word {
            "and" => Some(Keyword::And),
            "class" => Some(Keyword::Class),
            "else" => Some(Keyword::Else),
            "false" => Some(Keyword::False),
            "true" => Some(Keyword::True),
            "for" => Some(Keyword::For),
            "proc" => Some(Keyword::Proc),
            "if" => Some(Keyword::If),
            "null" => Some(Keyword::Null),
            "or" => Some(Keyword::Or),
            "return" => Some(Keyword::Return),
            "super" => Some(Keyword::Super),
            "this" => Some(Keyword::This),
            "let" => Some(Keyword::Let),
            "const" => Some(Keyword::Const),
            "while" => Some(Keyword::While),
            _ => None,
        }
    }
}

#[allow(clippy::upper_case_acronyms)]
enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Slash,
    Whitespace,
    Newline,
    DoubleQuote,
    SingleQuote,
    Identifier(String),
    Keyword(Keyword),
    Float(f64),
    Integer(i64),
    UnrecognizedToken,
    EOF,
}

// impl TokenKind {
//     fn from_char(lexeme: char) -> Self {
//         match lexeme {
//             '(' => TokenKind::LeftParen,
//             ')' => TokenKind::RightParen,
//             '{' => TokenKind::LeftBrace,
//             '}' => TokenKind::RightBrace,
//             ',' => TokenKind::Comma,
//             '.' => TokenKind::Dot,
//             '-' => TokenKind::Minus,
//             '+' => TokenKind::Plus,
//             ';' => TokenKind::Semicolon,
//             '*' => TokenKind::Star,
//             '!' => TokenKind::Bang,
//             '=' => TokenKind::Equal,
//             '<' => TokenKind::Less,
//             '>' => TokenKind::Greater,
//             '/' => TokenKind::Slash,
//             ' ' => TokenKind::Whitespace,
//             '\n' => TokenKind::Newline,
//             '0'..='9' => TokenKind::Digit,
//             '"' => TokenKind::DoubleQuote,
//             '\'' => TokenKind::SingleQuote,
//             c if c.is_alphabetic() => TokenKind::Identifier("placeholder".to_string()),
//             _ => TokenKind::UnrecognizedToken,
//         }
//     }
// }
