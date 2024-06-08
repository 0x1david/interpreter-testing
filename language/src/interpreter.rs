use std::{fmt::Display, ops::Neg};

use crate::{environment::Environment, expression::{Binary, BinaryOpToken, Expr, Literal, Object, Unary, UnaryOpToken}, lexer::TokenKind, statement::{Expression, Let, Print, Statement, Variable}};

type Result = std::result::Result<Value, String>;


#[derive(Clone)]
pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Nil,
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(s) => write!(f, "String: {}", s),
            Self::Integer(i) => write!(f, "Integer: {}", i),
            Self::Float(n) => write!(f, "Float: {}", n),
            Self::Nil => write!(f, "Nil"),
            Self::Bool(b) => write!(f, "Bool: {}", b),
        }
    }
}

pub struct Interpreter {
    environment: Environment
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            environment: Environment::new()
        }

    }
    pub fn interpret() {
        unimplemented!()
    }

    pub fn interpret_expr(&self, e: Expr) -> std::result::Result<Value, String> {
        let value = match e {
            Expr::Literal(expr) => self.interpret_literal(expr),
            Expr::Binary(expr) => self.interpret_binary(expr)?,
            Expr::Unary(expr) => self.interpret_unary(expr)?,
            Expr::Variable(expr) => self.interpret_var(expr)?,
            _ => return Err("Unimplemented expression type".to_string()),
        };
        Ok(value)
    }
    pub fn interpret_stmt(&mut self, e: Statement) {
        match e {
            Statement::Let(stmt) => self.interpret_assignment(stmt),
            Statement::Print(stmt) => self.interpret_print(stmt),
            Statement::Expression(expr) => self.interpret_expr_stmt(expr),
            _ => panic!("Unimplemented statement type"),
        };
    }

    pub fn interpret_expr_stmt(&self, e: Expression) {
        let _ = self.interpret_expr(e.expression).expect("Lazy as hell");
    }

    pub fn interpret_var(&self, e: crate::expression::Variable) -> Result {
        self.environment.read(&e.name)
    }

    pub fn interpret_print(&self, e: Print) {
        let expr = self.interpret_expr(e.expression);
        println!("{}", expr.expect("Currently lazy to even come up with text to write."))
    }
    pub fn interpret_assignment(&mut self, e: Let) {
        let val = self.interpret_expr(e.initializer).unwrap();
        let name = match dbg!(e.name.ttype) {
            TokenKind::Identifier(s) => s,
            _ => panic!("Assignment should never have a name that is not a string")
        };
        self.environment.define(name, val);
    }

    pub fn interpret_literal(&self, e: Literal) -> Value {
        match e.value {
            Object::True => Value::Bool(true),
            Object::False => Value::Bool(false),
            Object::Integer(i) => Value::Integer(i),
            Object::Float(f) => Value::Float(f),
            Object::String(s) => Value::String(s),
            Object::Null => Value::Nil,
        }
    }
    pub fn interpret_binary(&self, e: Binary) -> Result {
        let lhs = self.interpret_expr(*e.lhs)?;
        let rhs = self.interpret_expr(*e.rhs)?;

        match (&lhs, &e.operator, &rhs) {
            (Value::String(s1), BinaryOpToken::Plus, Value::String(s2)) => {
                Ok(Value::String(format!("{}{}", s1, s2)))
            }
            (Value::Integer(n1), BinaryOpToken::Plus, Value::Integer(n2)) => {
                Ok(Value::Integer(n1 + n2))
            }
            (Value::Integer(n1), BinaryOpToken::Minus, Value::Integer(n2)) => {
                Ok(Value::Integer(n1 - n2))
            }
            (Value::Integer(n1), BinaryOpToken::Slash, Value::Integer(n2)) => {
                Ok(Value::Integer(n1 / n2))
            }
            (Value::Integer(n1), BinaryOpToken::Star, Value::Integer(n2)) => {
                Ok(Value::Integer(n1 * n2))
            }
            (Value::Integer(n1), BinaryOpToken::NotEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 != n2))
            }
            (Value::Integer(n1), BinaryOpToken::EqualEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 == n2))
            }
            (Value::Integer(n1), BinaryOpToken::Greater, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 > n2))
            }
            (Value::Integer(n1), BinaryOpToken::GreaterEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 >= n2))
            }
            (Value::Integer(n1), BinaryOpToken::Less, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 < n2))
            }
            (Value::Integer(n1), BinaryOpToken::LessEqual, Value::Integer(n2)) => {
                Ok(Value::Bool(n1 <= n2))
            }
            _ => Err(format!(
                "Incorrect arguments found for a Binary Expression, lhs: {}, operator: {}, rhs: {}",
                lhs, e.operator, rhs
            )),
        }
    }
    fn interpret_unary(&self, e: Unary) -> Result {
        let rhs = self.interpret_expr(*e.value)?;
        let operator = e.operator;

        match (&operator, &rhs) {
            (UnaryOpToken::Minus, Value::Integer(n)) => Ok(Value::Integer(n.neg())),
            (UnaryOpToken::Bang, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid arguments found for a Unary Expression: operator: {operator}, value: {rhs}"))
        }
    }
}
