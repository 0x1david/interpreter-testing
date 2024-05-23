
use crate::expression::{Token, Variable};

pub enum Statement {
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub struct Struct {
    pub name: Token,
    pub superclass: Variable,
    pub body: Block,
}

pub struct Expression {
    pub expression: Expr,
}

pub struct Procedure {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

pub struct If {
    pub condition: Expr,
    pub then_branch: Statement,
    pub else_branch: Statement,
}

pub struct Print {
    pub expression: Expr,
}

pub struct Return {
    pub keyword: Token,
    pub value: Expr,
}

pub struct Let {
    pub name: Token,
    pub initializer: Expr,
}


pub struct While{
    pub condition: Expression,
    pub body: Statement,
}

pub struct Expr {}
