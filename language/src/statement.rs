use crate::expression::{Expr, Variable};
use crate::lexer::{Keyword, TokenKind};
use crate::parser::Parser;
use crate::token::Token;

#[allow(dead_code)]
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
}

impl Parser {
    pub fn parse_statement(&mut self) -> Option<Statement> {
        let tokenkind = &self.consume().ttype;
        let kw = tokenkind.keyword();

        if let Some(kw) = kw {
            match kw {
                Keyword::Let => self.parse_let(),
                Keyword::Print => self.parse_print(),
                _ => unimplemented!(),
            }
        } else {
            self.parse_expression_stmt()
        }
    }
    fn parse_print(&mut self) -> Option<Statement> {
        self.step();
        let value = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Print(Print { expression: value }))
    }

    fn parse_let(&mut self) -> Option<Statement> {
        let identifier = self.consume().clone();

        let value = if self.consume().ttype == TokenKind::Equal {
            self.parse_expression()
        } else {
            panic!("Expected an equal during an assignment parsing after let.")
        };

        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };

        Some(Statement::Let(Let {
            name: identifier,
            initializer: value,
        }))
    }
    fn parse_expression_stmt(&mut self) -> Option<Statement> {
        dbg!("Parsing exp: ", &self.peek().ttype);
        let expr = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Expression(Expression { expression: expr }))
    }
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Token,
    pub superclass: Variable,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct Procedure {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub else_branch: Box<Statement>,
}

#[derive(Debug, Clone)]
pub struct Print {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Let {
    pub name: Token,
    pub initializer: Expr,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub body: Box<Statement>,
}
