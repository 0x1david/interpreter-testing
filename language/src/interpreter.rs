use std::ops::Deref;

use crate::expression::{Binary, BinaryOpToken, Expr, Literal, Object};



type Result = std::result::Result<Value, String>;

pub enum Value {
    String(String),
    Integer(i64),
    Float(f64),
    Nil,
    Bool(bool),
}

pub struct Interpreter {}


impl Interpreter {
    pub fn interpret() { unimplemented!() }

    pub fn interpret_expr<Exp: Deref<Target = Expr>>(e: Exp) -> Result { unimplemented!() }

    pub fn interpret_literal(e: Literal) -> Value {
        match e.value { 
            Object::True => Value::Bool(true),
            Object::False=> Value::Bool(false),
            Object::Integer(i) => Value::Integer(i),
            Object::Float(f) => Value::Float(f),
            Object::String(s) => Value::String(s),
            Object::Null => Value::Nil,
        }
    }
    pub fn interpret_binary(e: Binary) -> Result {
        let lhs = Self::interpret_expr(e.lhs)?;
        let rhs = Self::interpret_expr(e.rhs)?;
        match (lhs, e.operator, rhs) {
            (Value::String(s1), BinaryOpToken::Plus, Value::String(s2))  => Ok(Value::String(format!("{}{}", s1, s2))),
            (Value::Integer(n1), BinaryOpToken::Plus, Value::Integer(n2))  => Ok(Value::Integer(n1+n2)),
            (Value::Integer(n1), BinaryOpToken::Minus, Value::Integer(n2))  => Ok(Value::Integer(n1-n2)),
            (Value::Integer(n1), BinaryOpToken::Slash, Value::Integer(n2))  => Ok(Value::Integer(n1/n2)),
            (Value::Integer(n1), BinaryOpToken::Star, Value::Integer(n2))  => Ok(Value::Integer(n1*n2)),
            (Value::Integer(n1), BinaryOpToken::NotEqual, Value::Integer(n2))  => Ok(Value::Bool(n1!=n2)),
            (Value::Integer(n1), BinaryOpToken::EqualEqual, Value::Integer(n2))  => Ok(Value::Bool(n1==n2)),
            (Value::Integer(n1), BinaryOpToken::Greater, Value::Integer(n2))  => Ok(Value::Bool(n1>n2)),
            (Value::Integer(n1), BinaryOpToken::GreaterEqual, Value::Integer(n2))  => Ok(Value::Bool(n1>=n2)),
            (Value::Integer(n1), BinaryOpToken::Less, Value::Integer(n2))  => Ok(Value::Bool(n1<n2)),
            (Value::Integer(n1), BinaryOpToken::LessEqual, Value::Integer(n2))  => Ok(Value::Bool(n1<=n2)),
            // Continue Here tommorow
        }
        
    }
}

