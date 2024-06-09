use std::{fmt::Display, ops::Neg};

use crate::{
    environment::Environment,
    expression::{Binary, BinaryOpToken, Expr, Literal, Object, Unary, UnaryOpToken},
    lexer::TokenKind,
    statement::{Expression, Let, Print, Statement, Variable},
};

type Result = std::result::Result<Value, String>;

/// Represents the possible values that can be stored in the interpreter's environment.
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

/// The main interpreter struct that holds the environment and interprets expressions and statements.
pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    /// Creates a new Interpreter instance with an empty environment.
    pub fn new() -> Self {
        Self {
            environment: Environment::new(),
        }
    }

    /// Placeholder for the main interpret function, yet to be implemented.
    pub fn interpret() {
        unimplemented!()
    }

    /// Interprets an expression and returns the resulting value or an error.
    ///
    /// # Arguments
    ///
    /// * `e` - The expression to interpret.
    ///
    /// # Returns
    ///
    /// A `Result` containing the value or an error message.
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

    /// Interprets a statement and executes it.
    ///
    /// # Arguments
    ///
    /// * `e` - The statement to interpret.
    pub fn interpret_stmt(&mut self, e: Statement) {
        match e {
            Statement::Let(stmt) => self.interpret_assignment(stmt),
            Statement::Print(stmt) => self.interpret_print(stmt),
            Statement::Expression(expr) => self.interpret_expr_stmt(expr),
            _ => panic!("Unimplemented statement type"),
        };
    }

    /// Interprets an expression statement.
    ///
    /// # Arguments
    ///
    /// * `e` - The expression statement to interpret.
    pub fn interpret_expr_stmt(&self, e: Expression) {
        let _ = self
            .interpret_expr(e.expression)
            .expect("Failed interpreting an expression statement.");
    }

    /// Interprets a variable expression and returns its value.
    ///
    /// # Arguments
    ///
    /// * `e` - The variable expression to interpret.
    ///
    /// # Returns
    ///
    /// A `Result` containing the value or an error message.
    pub fn interpret_var(&self, e: crate::expression::Variable) -> Result {
        self.environment.read(&e.name)
    }

    /// Interprets a print statement and outputs the result.
    ///
    /// # Arguments
    ///
    /// * `e` - The print statement to interpret.
    pub fn interpret_print(&self, e: Print) {
        let expr = self.interpret_expr(e.expression);
        println!(
            "{}",
            expr.expect("Failed interpreting an expression statement")
        )
    }

    /// Interprets an assignment statement and updates the environment.
    ///
    /// # Arguments
    ///
    /// * `e` - The assignment statement to interpret.
    pub fn interpret_assignment(&mut self, e: Let) {
        let val = self.interpret_expr(e.initializer).unwrap();
        let name = match dbg!(e.name.ttype) {
            TokenKind::Identifier(s) => s,
            _ => panic!("Assignment should never have a name that is not a string"),
        };
        self.environment.define(name, val);
    }

    /// Interprets a literal expression and returns its value.
    ///
    /// # Arguments
    ///
    /// * `e` - The literal expression to interpret.
    ///
    /// # Returns
    ///
    /// The value of the literal expression.
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

    /// Interprets a binary expression and returns its value or an error.
    ///
    /// # Arguments
    ///
    /// * `e` - The binary expression to interpret.
    ///
    /// # Returns
    ///
    /// A `Result` containing the value or an error message.
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

    /// Interprets a unary expression and returns its value or an error.
    ///
    /// # Arguments
    ///
    /// * `e` - The unary expression to interpret.
    ///
    /// # Returns
    ///
    /// A `Result` containing the value or an error message.
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
