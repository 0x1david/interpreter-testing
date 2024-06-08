use std::fmt::Display;

use crate::{expression::Literal, lexer::TokenKind};
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
            None => panic!("Can't print None")
        }
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

impl Token {
    pub fn new(ttype: TokenKind, literal: Option<Literal>, line: usize) -> Self {
        Self {
            ttype,
            literal,
            line,
            span: Span { start: 0, end: 0 },
        }
    }
    pub fn equality(&self) -> bool {
        matches!(&self.ttype, TokenKind::EqualEqual | TokenKind::BangEqual)
    }
    pub fn comparison(&self) -> bool {
        matches!(
            &self.ttype,
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual
        )
    }
    pub fn term(&self) -> bool {
        matches!(&self.ttype, TokenKind::Plus | TokenKind::Minus)
    }
    pub fn factor(&self) -> bool {
        matches!(&self.ttype, TokenKind::Star | TokenKind::Slash)
    }
    pub fn unary(&self) -> bool {
        matches!(&self.ttype, TokenKind::Bang | TokenKind::Minus)
    }
}
