#![allow(dead_code)]
use std::{
    fmt::Display,
    ops::{Deref, Neg},
};

use crate::expression::{Binary, BinaryOpToken, Expr, Literal, Object, Unary, UnaryOpToken};

type Result = std::result::Result<Value, String>;

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

pub struct Interpreter {}

impl Interpreter {
    pub fn interpret() {
        unimplemented!()
    }

    pub fn interpret_expr(e: Expr) -> std::result::Result<Value, String> {
        let value = match e {
            Expr::Literal(expr) => Self::interpret_literal(expr),
            Expr::Binary(expr) => Self::interpret_binary(expr)?,
            Expr::Unary(expr) => Self::interpret_unary(expr)?,
            _ => return Err("Unimplemented expression type".to_string()),
        };
        Ok(value)
    }

    pub fn interpret_literal(e: Literal) -> Value {
        match e.value {
            Object::True => Value::Bool(true),
            Object::False => Value::Bool(false),
            Object::Integer(i) => Value::Integer(i),
            Object::Float(f) => Value::Float(f),
            Object::String(s) => Value::String(s),
            Object::Null => Value::Nil,
        }
    }
    pub fn interpret_binary(e: Binary) -> Result {
        let lhs = Self::interpret_expr(*e.lhs)?;
        let rhs = Self::interpret_expr(*e.rhs)?;

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
    fn interpret_unary(e: Unary) -> Result {
        let rhs = Self::interpret_expr(*e.value)?;
        let operator = e.operator;

        match (&operator, &rhs) {
            (UnaryOpToken::Minus, Value::Integer(n)) => Ok(Value::Integer(n.neg())),
            (UnaryOpToken::Bang, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid arguments found for a Unary Expression: operator: {operator}, value: {rhs}"))
        }
    }
}
