use crate::expression::{Expr, Variable};
use crate::lexer::Keyword;
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
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.consume().ttype {
            Keyword::Print => self.parse_print(),
            Keyword::Let => self.parse_let(),
            _ => unimplemented!()
            
        }
    }
    fn parse_print(&mut self) -> Option<Statement> {
        self.step();
        let value = self.parse_expression();
        Some(Statement::Print(Print{ expression: value}))
    }
    ///TODO: Assignment requires a lot of further special handling 
    fn parse_let(&mut self) -> Option<Statement> {
        let var = self.peek().clone();
        self.step();
        let value = self.parse_expression();
        Some(Statement::Let(Let { name: var, initializer: value}))
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
