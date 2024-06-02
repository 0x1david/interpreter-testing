use crate::{
    expression::Expr, interpreter::{self, Interpreter}, lexer::{Lexer, TokenKind}, statement::{Expression, Statement}, token::Token
};

/// The `Parser` struct is responsible for parsing a sequence of tokens into a syntax tree.
/// It keeps track of the current position in the token sequence and maintains a list of parsed statements.
pub struct Parser {
    /// A vector of tokens to be parsed.
    pub tokens: Vec<Token>,
    /// The current position in the token vector.
    current: usize,
    /// A vector of parsed statements.
    statements: Vec<Statement>,
}

impl Parser {
    /// Creates a new `Parser` instance from a `Lexer` instance.
    ///
    /// # Arguments
    /// * `lexer` - A `Lexer` instance that will be consumed to provide the tokens to be parsed.
    pub fn new(lexer: Lexer) -> Self {
        Self {
            current: 0,
            tokens: lexer.get_all_tokens(),
            statements: vec![],
        }
    }
    /// Creates a new `Parser` instance from a vector of tokens.
    ///
    /// # Arguments
    /// * `tokens` - A vector of tokens to be parsed.
    pub fn from_token_vec(tokens: Vec<Token>) -> Self {
        Self {
            current: 0,
            tokens,
            statements: vec![],
        }
    }

    /// Advances the current position by one token.
    pub fn step(&mut self) {
        if !self.is_at_end() {
            self.current += 1;
        }
    }

    /// Adds an expression to the list of statements as an expression statement.
    ///
    /// # Arguments
    /// * `expr` - The expression to be added.
    pub fn add_expression(&mut self, expr: Expr) {
        self.add_statement(Statement::Expression(Expression { expression: expr }))
    }

    /// Adds a statement to the list of statements.
    ///
    /// # Arguments
    /// * `statement` - The statement to be added.
    pub fn add_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }

    /// Returns the token immediately before the current token.
    ///
    /// # Returns
    /// A reference to the previous token.
    pub fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    /// Returns the current token being parsed.
    ///
    /// # Returns
    /// A reference to the current token.
    pub fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    /// Consumes the current token and advances to the next token.
    ///
    /// # Returns
    /// A reference to the consumed token.
    ///
    /// # Panics
    /// Panics if called when the parser is at the end of the token list.
    // TODO: Properly implement error catching for consume (Should check for if the value is of a
    // passed type)
    pub fn consume(&mut self) -> &Token {
        self.step();
        let tik = self.previous();
        tik
    }

    /// Checks if the parser has reached the end of the token list.
    ///
    /// # Returns
    /// `true` if the current token is of type `EOF`, otherwise `false`.
    pub fn is_at_end(&self) -> bool {
        self.tokens[self.current].ttype == TokenKind::EOF
    }

    /// Parses the tokens into a list of statements.
    pub fn parse(&mut self){
        println!("now parsing");
        while !self.is_at_end() {
            let exp = self.parse_expression();
            let printr_expr = exp.clone();
            println!("Parse next");
            let interpreted_exp = Interpreter::interpret_expr(exp);
            println!("Value of interpreted expression:{:?} \n is {}", printr_expr, &interpreted_exp.unwrap());

        }
    }
    /// Returns the type of the current token.
    ///
    /// # Returns
    ///
    /// A reference to the type of the current token.
    pub fn token_type(&self) -> &TokenKind {
        &self.tokens[self.current].ttype
    }
}
