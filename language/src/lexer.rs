use anyhow::Result;
use std::char;

use crate::token::Token;

#[derive(Debug)]
pub struct Lexer<'a> {
    source: &'a str,
    pub tokens: Vec<Token>,
    chars: std::iter::Peekable<std::str::Chars<'a>>,

    start: usize,
    current: usize,
    line: usize,
}

/// The `Lexer` struct is responsible for tokenizing a source string into a series of tokens.
/// It maintains the state of the current position in the source code, the start of the current lexeme,
/// and the current line number for error reporting.
///
/// # Fields
/// - `source`: A string slice containing the source code to be tokenized.
/// - `tokens`: A vector of `TokenKind` representing the tokens generated from the source.
/// - `chars`: A peekable iterator over the characters of the source string.
/// - `start`: The start position of the current lexeme.
/// - `current`: The current position in the source string.
/// - `line`: The current line number in the source string.
///
impl<'a> Lexer<'a> {
    /// Creates a new `Lexer` instance from the given source string.
    ///
    /// # Parameters
    /// - `source`: A string slice containing the source code to be lexed.
    ///
    /// # Returns
    /// - A `Lexer` instance.
    pub fn new(source: &'a str) -> Self {
        Self {
            chars: source.chars().peekable(),
            source,
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }
    /// Adds a token to the lexer's list of tokens.
    ///
    /// # Parameters
    /// - `token`: The token to be added.
    ///
    fn add_token(&mut self, token: TokenKind) {
        self.tokens.push(Token::new(token, None, self.line))
    }

    /// Checks if the lexer has reached the end of the source input.
    ///
    /// # Returns
    /// - `true` if at the end of the input, otherwise `false`.
    ///
    fn is_at_end(&mut self) -> bool {
        self.chars.peek().is_none()
    }

    /// Returns a reference to the next character in the source input without advancing the current position.
    ///
    /// # Returns
    /// - `Some(&char)` if there is a next character, otherwise `None`.
    ///
    fn peek(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    /// Advances the iterator and returns the current character.
    ///
    /// # Returns
    /// - The current character.
    ///
    /// # Panics
    /// - If the end of the input is reached without checking for EOF.
    ///
    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars
            .next()
            .expect("We are looking for EOF before advancing.")
    }

    /// Collects characters until the provided condition is met.
    ///
    /// # Parameters
    /// - `f`: A closure that takes a character and returns `true` if the condition is met.
    ///
    /// # Returns
    /// - A string of collected characters.
    fn collect_until<F: Fn(char) -> bool>(&mut self, f: F) -> String {
        let mut collected = String::default();
        while let Some(&ch) = self.peek() {
            if f(ch) {
                break;
            };
            collected.push(ch);
            self.advance();
        }
        collected
    }

    /// Collects characters until the provided condition is met and returns a `Result`.
    ///
    /// # Parameters
    /// - `f`: A closure that takes a character and returns `true` if the condition is met.
    ///
    /// # Returns
    /// - `Ok(String)` if the condition is met.
    /// - `Err(anyhow::Error)` if the end of input is reached without meeting the condition.
    fn try_collect_until<F: Fn(char) -> bool>(&mut self, f: F) -> Result<String> {
        let mut collected = String::default();
        while let Some(&ch) = self.peek() {
            if f(ch) {
                return Ok(collected);
            };
            collected.push(ch);
            self.advance();
        }
        anyhow::bail!("Unterminated string");
    }

    /// Advances the iterator until the provided condition is met.
    ///
    /// # Parameters
    /// - `f`: A closure that takes a character and returns `true` if the condition is met.
    fn advance_until<F: Fn(char) -> bool>(&mut self, f: F) {
        while let Some(&ch) = self.peek() {
            if f(ch) {
                break;
            }
            self.advance();
        }
    }

    pub fn get_all_tokens(self) -> Vec<Token> {
        self.tokens
    }

    /// Scans the next token from the source input and adds it to the list of tokens.
    ///
    /// This function handles different types of tokens, including:
    /// - Single-character tokens (e.g., parentheses, braces).
    /// - Multi-character tokens (e.g., `!=`, `==`).
    /// - Comments (e.g., `// comment`).
    /// - Whitespace and newlines.
    /// - Numeric literals (both integers and floats).
    /// - String literals (enclosed in double or single quotes).
    /// - Identifiers and keywords.
    fn scan_token(&mut self) {
        let ch = self.advance();
        match ch {
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
                    self.advance_until(|x| x == '\n');
                    self.advance();
                    self.line += 1;
                } else {
                    self.add_token(TokenKind::Slash);
                }
            }
            ' ' | '\r' | '\t' => {}
            '\n' => self.line += 1,
            '0'..='9' => {
                let mut number = String::new();
                number.push(ch);
                let digits = self.collect_until(|c| !c.is_numeric() && c != '.');
                number.push_str(&digits);

                match number.parse::<i64>() {
                    Ok(int) => self.add_token(TokenKind::Integer(int)),
                    Err(_) => match number.parse::<f64>() {
                        Ok(flt) => self.add_token(TokenKind::Float(flt)),
                        Err(_) => {
                            panic!("Number should always be parsable as either integer or float.")
                        }
                    },
                }
            }
            '"' | '\'' => {
                let string = self
                    .try_collect_until(|c| c == ch)
                    .expect("TODO: Unimplemented err");
                self.add_token(TokenKind::String(string));
                self.advance();
            }
            c if c.is_alphabetic() => {
                let letter = c.to_string();
                let string = self.collect_until(|c| !c.is_alphanumeric() && c != '_');
                let string = letter + &string;

                let keyword = Keyword::from_word(&string);
                match keyword {
                    Some(keyword) => self.add_token(TokenKind::Keyword(keyword)),
                    None => self.add_token(TokenKind::Identifier(string)),
                }
            }
            _ => self.add_token(TokenKind::UnrecognizedToken),
        };
    }

    /// Scans all tokens from the source input until the end is reached and populates the lexer's list of tokens.
    ///
    /// This function repeatedly calls `scan_token` to process each token in the input source until the end of the source is reached.
    ///
    pub fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }
        self.add_token(TokenKind::EOF)
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents all current keywords in the source language.
pub enum Keyword {
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
    Print,
}

impl Keyword {
    /// Converts a string slice to the corresponding `Keyword` enum variant, if any.
    ///
    /// # Parameters
    /// - `word`: A string slice representing a potential keyword.
    ///
    /// # Returns
    /// - `Some(Keyword)` if the word matches a keyword.
    /// - `None` if the word does not match any keyword.
    ///
    fn from_word(word: &str) -> Option<Self> {
        match word {
            "print" => Some(Keyword::Print),
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
#[derive(PartialEq, Debug, Clone)]
/// Represents the current kinds of tokens that can be found in the source language.
pub(crate) enum TokenKind {
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
    String(String),
    SingleQuote,
    Identifier(String),
    Keyword(Keyword),
    Float(f64),
    Integer(i64),
    UnrecognizedToken,
    EOF,
}

impl TokenKind {
    pub fn keyword(&self) -> Option<&Keyword> {
        match self {
            TokenKind::Keyword(kw) => Some(kw),
            _ => None,
        }
    }
    pub fn string(&self) -> Option<&str> {
        match self {
            TokenKind::String(s) => Some(s),
            _ => None,
        }
    }
    pub fn identifier(&self) -> Option<&str> {
        match self {
            TokenKind::Identifier(id) => Some(id),
            _ => None,
        }
    }
    pub fn float(&self) -> Option<f64> {
        match self {
            TokenKind::Float(f) => Some(*f),
            _ => None,
        }
    }
    pub fn integer(&self) -> Option<i64> {
        match self {
            TokenKind::Integer(i) => Some(*i),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_new() {
        let source = "let x = 42;";
        let lexer = Lexer::new(source);
        assert_eq!(lexer.source, source);
        assert_eq!(lexer.tokens.len(), 0);
        assert_eq!(lexer.start, 0);
        assert_eq!(lexer.current, 0);
        assert_eq!(lexer.line, 1);
    }

    #[test]
    fn test_lexer_is_at_end() {
        let source = "";
        let mut lexer = Lexer::new(source);
        assert!(lexer.is_at_end());
    }

    #[test]
    fn test_lexer_peek() {
        let source = "let";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.peek(), Some(&'l'));
    }

    #[test]
    fn test_lexer_advance() {
        let source = "let";
        let mut lexer = Lexer::new(source);
        assert_eq!(lexer.advance(), 'l');
        assert_eq!(lexer.current, 1);
    }

    #[test]
    fn test_lexer_collect_until() {
        let source = "let x = 42;";
        let mut lexer = Lexer::new(source);
        let collected = lexer.collect_until(|c| c == ' ');
        assert_eq!(collected, "let");
        assert_eq!(lexer.current, 3);
    }

    #[test]
    fn test_lexer_try_collect_until() {
        let source = "\"hello\"";
        let mut lexer = Lexer::new(source);
        lexer.advance(); // Skip the opening quote
        let result = lexer.try_collect_until(|c| c == '"');
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "hello");
    }

    #[test]
    fn test_lexer_scan_token() {
        let source = "let x = 42;";
        let mut lexer = Lexer::new(source);
        lexer.scan_token();
        assert_eq!(lexer.tokens.len(), 1);

        if let TokenKind::Keyword(ref id) = lexer.tokens[0].ttype {
            assert_eq!(id, &Keyword::Let);
        } else {
            panic!("Expected a Keyword token.");
        }
    }

    #[test]
    fn test_lexer_scan_tokens() {
        let source = "let x = 42;";
        let mut lexer = Lexer::new(source);
        lexer.scan_tokens();
        assert_eq!(lexer.tokens.len(), 6);
        assert_eq!(lexer.tokens[0].ttype, TokenKind::Keyword(Keyword::Let));
        assert_eq!(
            lexer.tokens[1].ttype,
            TokenKind::Identifier("x".to_string())
        );
        assert_eq!(lexer.tokens[2].ttype, TokenKind::Equal);
        assert_eq!(lexer.tokens[3].ttype, TokenKind::Integer(42));
        assert_eq!(lexer.tokens[4].ttype, TokenKind::Semicolon);
        assert_eq!(lexer.tokens[5].ttype, TokenKind::EOF);
    }

    #[test]
    fn test_lexer_scan_tokens_str() {
        let source = "let word = 'apples';";
        let mut lexer = Lexer::new(source);
        lexer.scan_tokens();
        assert_eq!(lexer.tokens.len(), 6);
        assert_eq!(lexer.tokens[0].ttype, TokenKind::Keyword(Keyword::Let));
        assert_eq!(
            lexer.tokens[1].ttype,
            TokenKind::Identifier("word".to_string())
        );
        assert_eq!(lexer.tokens[2].ttype, TokenKind::Equal);
        assert_eq!(
            lexer.tokens[3].ttype,
            TokenKind::String("apples".to_string())
        );
        assert_eq!(lexer.tokens[4].ttype, TokenKind::Semicolon);
        assert_eq!(lexer.tokens[5].ttype, TokenKind::EOF);
    }

    #[test]
    fn test_full_file_parse() {
        let source = r#"
            let x = 42;
            let y = 3.22;
            let message = "Hello, World!";
            let char_literal = 'c';
            const MAX = 100;
            if x > 10 {
                println("x is greater than 10");
            } else {
                println("x is 10 or less");
            }
            while y < MAX {
                y = y + 1;
            }
        "#;

        let mut lexer = Lexer::new(source);
        lexer.scan_tokens();

        let expected_tokens = vec![
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Equal,
            TokenKind::Integer(42),
            TokenKind::Semicolon,
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Equal,
            TokenKind::Float(3.22),
            TokenKind::Semicolon,
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("message".to_string()),
            TokenKind::Equal,
            TokenKind::String("Hello, World!".to_string()),
            TokenKind::Semicolon,
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier("char_literal".to_string()),
            TokenKind::Equal,
            TokenKind::String("c".to_string()),
            TokenKind::Semicolon,
            TokenKind::Keyword(Keyword::Const),
            TokenKind::Identifier("MAX".to_string()),
            TokenKind::Equal,
            TokenKind::Integer(100),
            TokenKind::Semicolon,
            TokenKind::Keyword(Keyword::If),
            TokenKind::Identifier("x".to_string()),
            TokenKind::Greater,
            TokenKind::Integer(10),
            TokenKind::LeftBrace,
            TokenKind::Identifier("println".to_string()),
            TokenKind::LeftParen,
            TokenKind::String("x is greater than 10".to_string()),
            TokenKind::RightParen,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Keyword(Keyword::Else),
            TokenKind::LeftBrace,
            TokenKind::Identifier("println".to_string()),
            TokenKind::LeftParen,
            TokenKind::String("x is 10 or less".to_string()),
            TokenKind::RightParen,
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::Keyword(Keyword::While),
            TokenKind::Identifier("y".to_string()),
            TokenKind::Less,
            TokenKind::Identifier("MAX".to_string()),
            TokenKind::LeftBrace,
            TokenKind::Identifier("y".to_string()),
            TokenKind::Equal,
            TokenKind::Identifier("y".to_string()),
            TokenKind::Plus,
            TokenKind::Integer(1),
            TokenKind::Semicolon,
            TokenKind::RightBrace,
            TokenKind::EOF,
        ];
        for (i, token) in expected_tokens.iter().enumerate() {
            assert_eq!(&lexer.tokens[i].ttype, token);
        }
    }
}
