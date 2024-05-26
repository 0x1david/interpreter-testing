use crate::{expression::Literal, lexer::TokenKind};
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct Token {
    pub ttype: TokenKind,
    literal: Option<Literal>,
    line: usize,
    column: usize,
}

impl Token {
    pub fn new(ttype: TokenKind, literal: Option<Literal>, line: usize, column: usize) -> Self {
        Self {
            ttype,
            literal,
            line,
            column,
        }
    }
    pub fn equality(&self) -> bool {
        match &self.ttype {
            TokenKind::EqualEqual | TokenKind::BangEqual => true,
            _ => false
        }
    }
    pub fn comparison(&self) -> bool {
        match &self.ttype {
            TokenKind::Greater | TokenKind::GreaterEqual | TokenKind::Less | TokenKind::LessEqual => true,
            _ => false
        }
    }
    pub fn term(&self) -> bool {
        match &self.ttype {
            TokenKind::Plus | TokenKind::Minus => true,
            _ => false
        }
    }
    pub fn factor(&self) -> bool {
        match &self.ttype {
            TokenKind::Star | TokenKind::Slash => true,
            _ => false
        }
    }
    pub fn unary(&self) -> bool {
        match &self.ttype {
            TokenKind::Bang | TokenKind::Minus => true,
            _ => false
        }
    }
}
