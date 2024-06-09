use crate::expression::{Expr, Literal};
use crate::lexer::{Keyword, TokenKind};
use crate::parser::Parser;
use crate::token::Token;

/// Represents different kinds of statements in the language.
#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block),
    Struct(Struct),
    Expression(Expression),
    Procedure(Procedure),
    Function(Function),
    If(If),
    Print(Print),
    Return(Return),
    Let(Let),
    While(While),
    Variable(Variable),
}

impl Parser {
    /// Parses a statement and returns an optional `Statement`.
    pub fn parse_statement(&mut self) -> Option<Statement> {
        let tokenkind = &self.consume().ttype.clone();
        dbg!("Parsing keyword");
        let kw = dbg!(tokenkind.keyword());

        if let Some(kw) = kw {
            match kw {
                Keyword::Let => self.parse_let(),
                Keyword::Print => self.parse_print(),
                _ => unimplemented!(),
            }
        } else if let Some(ident) = tokenkind.identifier() {
            self.parse_variable_expression(ident.to_string())
        } else if tokenkind.left_brace() {
            self.parse_block()
        }
        else {
            self.parse_expression_stmt()
        }
    }

    /// Parses a print statement and returns an optional `Statement`.
    fn parse_print(&mut self) -> Option<Statement> {
        dbg!("parsing print statement");
        let value = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Print(Print { expression: value }))
    }

    /// Parses a let statement and returns an optional `Statement`.
    fn parse_let(&mut self) -> Option<Statement> {
        let identifier = self.consume().clone();

        let value = if self.consume().ttype == TokenKind::Equal {
            self.parse_expression()
        } else {
            panic!("Expected an equal during an assignment parsing after let.")
        };

        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after let statement, proper error handling is TBD.")
        };

        dbg!("Returning form let parsing");
        dbg!(Some(Statement::Let(Let {
            name: identifier,
            initializer: value,
        })))
    }

    /// Parses a block and returns an optional `Statement`.
    fn parse_block(&mut self) -> Option<Statement> {
        let mut stmts = vec![];
        while !self.peek().ttype.right_brace() && !self.is_at_end() {
            dbg!("PARSING A BLOCK");
            stmts.push(self.parse_statement().expect("Found something else than a statement in a block."));
        }

        dbg!("fINISHED PARSING A BLOCK");

        if self.peek().ttype.right_brace() {
            self.consume();
            Some(Statement::Block(Block { statements: stmts }))
        } else {
            panic!("Unterminated block.")
        }

    }
    /// Parses an expression statement and returns an optional `Statement`.
    fn parse_expression_stmt(&mut self) -> Option<Statement> {
        dbg!("Parsing exp: ", &self.peek().ttype);
        let expr = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Expression(Expression { expression: expr }))
    }

    /// Parses a variable expression and returns an optional `Statement`.
    fn parse_variable_expression(&mut self, ident: String) -> Option<Statement> {
        println!("IN VARIABLE = {:?}", self.peek());
        Some(Statement::Variable(Variable {
            name: ident.to_string(),
        }))
    }
}

/// Represents a block of statements.
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

/// Represents a struct declaration.
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Token,
    pub superclass: Variable,
    pub body: Block,
}

/// Represents an expression statement.
#[derive(Debug, Clone)]
pub struct Expression {
    pub expression: Expr,
}

/// Represents a procedure declaration.
#[derive(Debug, Clone)]
pub struct Procedure {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

/// Represents a function declaration.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

/// Represents an if statement.
#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Box<Statement>,
}

/// Represents a print statement.
#[derive(Debug, Clone)]
pub struct Print {
    pub expression: Expr,
}

/// Represents a return statement.
#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Expr,
}

/// Represents a let statement.
#[derive(Debug, Clone)]
pub struct Let {
    pub name: Token,
    pub initializer: Expr,
}

/// Represents a while loop statement.
#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub body: Box<Statement>,
}

/// Represents a variable expression.
#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
}
