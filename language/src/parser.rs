use crate::{expression::Expr, lexer::{Lexer, TokenKind}, statement::{Expression, Statement}, token::Token};



pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    statements: Vec<Statement>
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            current: 0,
            tokens: lexer.get_all_tokens(),
            statements: vec![],
        }
    }

    pub fn step(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    pub fn add_expression(&mut self, expr: Expr) {
        self.add_statement(Statement::Expression(Expression{expression: expr}))
    }

    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }

    pub fn previous(&self) -> &Token {
        &self.tokens[self.current-1]
    }
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    pub fn consume(&self) -> &Token {
        self.step();
        &self.tokens[self.current]
    }
    pub fn is_at_end(&self) -> bool {
        self.tokens[self.current].ttype == TokenKind::EOF
    }
    pub fn parse(&mut self) {
        while !self.is_at_end() {}

    }
    pub fn token_type(&self) -> &TokenKind {
        &self.tokens[self.current].ttype
    }
}
