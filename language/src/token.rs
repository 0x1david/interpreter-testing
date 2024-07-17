use std::fmt::Display;

use crate::{
    expression::Literal,
    lexer::{Keyword, TokenKind},
};

/// Represents a token in the language with its type, literal value, span, and line number.
#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenKind,
    pub literal: Option<Literal>,
    pub span: Span,
    pub line: usize,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.literal {
            Some(val) => write!(f, "{}", val),
            None => panic!("Can't print None"),
        }
    }
}

/// Represents the span of a token in the source code.
#[derive(Debug, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    /// Creates a new span with the given start and end positions.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Token {
    /// Creates a new token with the given type, literal value, and line number.
    pub fn new(ttype: TokenKind, literal: Option<Literal>, line: usize) -> Self {
        Self {
            ttype,
            literal,
            line,
            span: Span { start: 0, end: 0 },
        }
    }

    /// Checks if the token represents an equality operator.
    pub fn equality(&self) -> bool {
        matches!(&self.ttype, TokenKind::EqualEqual | TokenKind::BangEqual)
    }

    pub fn and(&self) -> bool {
        matches!(&self.ttype, TokenKind::Keyword(Keyword::And))
    }

    pub fn or(&self) -> bool {
        matches!(&self.ttype, TokenKind::Keyword(Keyword::Or))
    }

    pub fn identifier(&self) -> bool {
        matches!(&self.ttype, TokenKind::Identifier(s))
    }

    pub fn get_identifier(&self) -> Option<String> {

        if matches!(&self.ttype, TokenKind::Identifier(s)) {
            Some(self.to_string())
        } else {
            None
        }
    }

    /// Checks if the token represents a comparison operator.
    pub fn comparison(&self) -> bool {
        matches!(
            &self.ttype,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        )
    }

    /// Checks if the token represents a term operator (addition or subtraction).
    pub fn term(&self) -> bool {
        matches!(&self.ttype, TokenKind::Plus | TokenKind::Minus)
    }

    /// Checks if the token represents a factor operator (multiplication or division).
    pub fn factor(&self) -> bool {
        matches!(&self.ttype, TokenKind::Star | TokenKind::Slash)
    }

    /// Checks if the token represents a unary operator (logical NOT or negation).
    pub fn unary(&self) -> bool {
        matches!(&self.ttype, TokenKind::Bang | TokenKind::Minus)
    }

    pub fn equal(&self) -> bool {
        matches!(&self.ttype, TokenKind::Equal)
    }
}
