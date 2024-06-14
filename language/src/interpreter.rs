use std::{cell::RefCell, fmt::Display, ops::Neg, rc::Rc};

use crate::{
    environment::Environment,
    expression::{
        Assign, Binary, BinaryOpToken, Expr, Literal, Logical, Object, Unary, UnaryOpToken, Variable
    },
    lexer::TokenKind,
    statement::{Block, Expression, If, Let, Print, Statement, While},
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

impl Value {
    pub fn bool_true(&self) -> bool {
        match self {
            Self::Bool(b) => *b,
            _ => false,
        }
    }
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
    pub environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    /// Creates a new Interpreter instance with an empty environment.
    pub fn new() -> Self {
        Self {
            environment: Rc::new(RefCell::new(Environment::new())),
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
    pub fn interpret_expr(&mut self, e: Expr) -> std::result::Result<Value, String> {
        let value = match e {
            Expr::Literal(expr) => self.interpret_literal(expr),
            Expr::Logical(expr) => self.interpret_logical(expr)?,
            Expr::Binary(expr) => self.interpret_binary(expr)?,
            Expr::Unary(expr) => self.interpret_unary(expr)?,
            Expr::Variable(expr) => self.interpret_var(expr)?,
            Expr::Assign(expr) => self.interpret_assign_expression(expr)?,
            _ => return Err(format!("Unimplemented expression type: {:?}", e)),
        };
        Ok(value)
    }

    /// Interprets a logical expression and returns the resulting boolean value or an error.
    ///
    /// # Arguments
    ///
    /// * `e` - The logical to interpret.
    ///
    /// # Returns
    ///
    /// A `Result` containing the value or an error message.
    pub fn interpret_logical(&mut self, e: Logical) -> std::result::Result<Value, String> {
        let value = if e.operator.and() {
            self.interpret_expr(*e.lhs).unwrap().bool_true()
                && self.interpret_expr(*e.rhs).unwrap().bool_true()
        } else if e.operator.or() {
            self.interpret_expr(*e.lhs).unwrap().bool_true()
                || self.interpret_expr(*e.rhs).unwrap().bool_true()
        } else {
            panic!("Only the logical operators (AND, OR) exist")
        };
        Ok(Value::Bool(value))
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
            Statement::Block(expr) => self.interpret_block(expr),
            Statement::Variable(expr) => self.interpret_var_stmt(expr),
            Statement::If(expr) => self.interpret_if_stmt(expr),
            Statement::While(expr) => self.interpret_while(expr),
            _ => panic!("Unimplemented statement type: {:?}", e),
        };
    }

    /// Interprets an expression statement.
    ///
    /// # Arguments
    ///
    /// * `e` - The expression statement to interpret.
    pub fn interpret_expr_stmt(&mut self, e: Expression) {
        let _ = self
            .interpret_expr(e.expression)
            .expect("Failed interpreting an expression statement.");
    }


    /// Interprets a WHILE statement.
    ///
    /// # Arguments
    ///
    /// * `e` - The while statement to interpret.
    pub fn interpret_while(&mut self, e: While) {
        while self.interpret_expr(e.condition.expression.clone()).expect("Failed to interpret condition of a while loop").bool_true() {
            self.interpret_stmt(*e.body.clone())
        }
    }

    /// Interprets a conditional 'If' statement..
    ///
    /// # Arguments
    ///
    /// * `e` - The if statement to interpret.
    pub fn interpret_if_stmt(&mut self, e: If) {
        if self
            .interpret_expr(e.condition)
            .expect("Couldn't interpret expression as boolean logic condition for a conditional statement.")
            .bool_true() {
                self.interpret_stmt(*e.then_branch)
        } else {
            for (cond, branch) in e.elif_branches {
                if self
                    .interpret_expr(cond)
                    .expect("Couldn't interpret expression as boolean logic condition for a conditional statement.")
                    .bool_true() {
                        self.interpret_stmt(branch);
                        return
                    }
            }
            if let Some(branch) = e.else_branch {
                self.interpret_stmt(*branch)
            }
        }
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
    pub fn interpret_var(&mut self, e: crate::expression::Variable) -> Result {
        if let Some(init) = e.initializer {
            let init = self.interpret_expr(*init).expect("Shouldn't fail intepreting rhs of assignemnt");
            self.environment.borrow_mut().assign(&e.name, init)?
        }
        self.environment.borrow().read(&e.name)
    }

    pub fn interpret_var_stmt(&mut self, e: Variable) {
        self.interpret_var(e);
    }
    /// Interprets a print statement and outputs the result.
    ///
    /// # Arguments
    ///
    /// * `e` - The print statement to interpret.
    pub fn interpret_print(&mut self, e: Print) {
        let expr = self.interpret_expr(e.expression);
        println!("PRINTING VALUE HERE");
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
    pub fn interpret_assign_expression(&mut self, e: Assign) -> Result {
        let val = self.interpret_expr(*e.value).unwrap();

        let name = match e.variable.ttype {
            TokenKind::Identifier(s) => s,
            _ => panic!("Assignment should never have a name that is not a string"),
        };
        self.environment.borrow_mut().assign(&name, val.clone());
        Ok(val)
    }

    /// Interprets an assignment statement and updates the environment.
    ///
    /// # Arguments
    ///
    /// * `e` - The assignment statement to interpret.
    pub fn interpret_assignment(&mut self, e: Let) {
        let val = self.interpret_expr(e.initializer).unwrap();
        let name = match e.name.ttype {
            TokenKind::Identifier(s) => s,
            _ => panic!("Assignment should never have a name that is not a string"),
        };
        self.environment.borrow_mut().define(name, val);
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
    pub fn interpret_binary(&mut self, e: Binary) -> Result {
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
            (Value::String(s1), BinaryOpToken::EqualEqual, Value::String(s2)) => {
                Ok(Value::Bool(s1 == s2))
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
    fn interpret_unary(&mut self, e: Unary) -> Result {
        let rhs = self.interpret_expr(*e.value)?;
        let operator = e.operator;

        match (&operator, &rhs) {
            (UnaryOpToken::Minus, Value::Integer(n)) => Ok(Value::Integer(n.neg())),
            (UnaryOpToken::Bang, Value::Bool(b)) => Ok(Value::Bool(!b)),
            _ => Err(format!("Invalid arguments found for a Unary Expression: operator: {operator}, value: {rhs}"))
        }
    }

    fn interpret_block(&mut self, e: Block) {
        let outer_environment = self.environment.clone();
        self.environment = Rc::new(RefCell::new(Environment::new_scoped(
            self.environment.clone(),
        )));
        for stmt in e.statements {
            self.interpret_stmt(stmt);
        }

        self.environment = outer_environment;
    }
}
