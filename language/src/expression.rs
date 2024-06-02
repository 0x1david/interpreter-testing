use std::{fmt::Display, ops::Neg};

use crate::{
    lexer::{Keyword, TokenKind},
    parser::Parser,
    token::Token,
};

impl Parser {
    /// Parses the current expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed expression.
    pub fn parse_expression(&mut self) -> Expr {
        dbg!("Parsing exp: ", &self.peek().ttype);
        self.parse_equality()
            .expect("After finishing, expression should be parsed")
    }
    /// Parses equality expressions (`==`, `!=`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed equality expression.
    pub fn parse_equality(&mut self) -> Option<Expr> {
        dbg!("Parsing eq: ", &self.peek().ttype);
        let mut expression = self.parse_comparison()?;
        while self.peek().equality() {
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_comparison()?;
            println!("The operator at equality is {:?}", operator.ttype);
            expression = Expr::binary(expression, operator, rhs);
        }
        Some(expression)
    }
    /// Parses comparison expressions (`<`, `<=`, `>`, `>=`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed comparison expression.
    pub fn parse_comparison(&mut self) -> Option<Expr> {
        dbg!("Parsing comparsion: ", &self.peek().ttype);
        let mut expression = self.parse_term()?;
        while self.peek().comparison() {
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_term()?;
            println!("The operator at comparison is {:?}", operator.ttype);
            expression = Expr::binary(expression, operator, rhs)
        }
        Some(expression)
    }
    /// Parses term expressions (`+`, `-`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed term expression.
    pub fn parse_term(&mut self) -> Option<Expr> {
        dbg!("Parsing term: ", &self.peek().ttype);
        let mut expression = self.parse_factor()?;
        dbg!("parsed at term now: ", &expression);
        while self.peek().term() {
            dbg!("I am at term parsing now");
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_factor()?;
            expression = Expr::binary(expression, operator, rhs)
        }
        Some(expression)
    }
    /// Parses factor expressions (`*`, `/`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed factor expression.
    pub fn parse_factor(&mut self) -> Option<Expr> {
        dbg!("Parsing factor: ", self.peek());
        let mut expression = self.parse_unary()?;
        while self.peek().factor() {
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_unary()?;
            expression = Expr::binary(expression, operator, rhs)
        }
        Some(expression)
    }
    /// Parses unary expressions (`!`, `-`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed unary expression.
    pub fn parse_unary(&mut self) -> Option<Expr> {
        dbg!("Parsing unary: ", &self.peek().ttype);
        if self.peek().unary() {
            self.step();
            let operator = match self.previous().ttype {
                TokenKind::Minus => UnaryOpToken::Minus,
                TokenKind::Bang => UnaryOpToken::Bang,
                _ => panic!("Not a valid expression"),
            };
            let rhs = self.parse_unary()?;
            return Some(Expr::unary(operator, rhs));
        }
        self.parse_primary()
    }
    /// Parses primary expressions (literals, grouped expressions, identifiers) or any lower
    /// priority expressions.
    ///
    /// # Returns
    /// An `Expr` representing the parsed primary expression.
    ///
    /// # Panics
    /// Panics if an invalid primary expression is encountered.
    // TODO: This code is hideous and duplicative, so rewrite later
    pub fn parse_primary(&mut self) -> Option<Expr> {
        dbg!("Parsing primary {}", &self.peek().ttype);
        if let Some(kw) = self.token_type().keyword() {
            match kw {
                Keyword::True => {
                    self.step();
                    return Some(Expr::literal(Object::True));
                }
                Keyword::False => {
                    self.step();
                    return Some(Expr::literal(Object::False));
                }
                Keyword::Null => {
                    self.step();
                    return Some(Expr::literal(Object::Null));
                }
                _ => return None,
            }
        };
        if let Some(i) = self.token_type().integer() {
            dbg!("REturning an integer");
            self.step();
            return Some(Expr::literal(Object::Integer(i)));
        };

        if let Some(f) = self.token_type().float() {
            self.step();
            return Some(Expr::literal(Object::Float(f)));
        };

        if self.token_type() == &TokenKind::LeftParen {
            self.step();
            let expr = self.parse_expression();
            if !(self.consume().ttype == TokenKind::RightParen) {
                panic!("Expected a right parenthesis after an expression starting with a left parenthesis got: {:?}.", &self.peek())
            }
            return Some(Expr::grouping(expr));
        };
        None
    }
}

/// The `Expr` enum represents different types of expressions in the syntax tree.
#[derive(Debug, Clone)]
pub enum Expr {
    Variable(Variable),
    This(This),
    Super(Super),
    Set(Set),
    Logical(Logical),
    Literal(Literal),
    Grouping(Grouping),
    Get(Get),
    Call(Call),
    Assign(Assign),
    Unary(Unary),
    Binary(Binary),
}

impl Expr {
    /// Creates a new binary expression.
    ///
    /// # Arguments
    ///
    /// * `lhs` - The left-hand side expression.
    /// * `operator` - The operator token.
    /// * `rhs` - The right-hand side expression.
    ///
    /// # Returns
    ///
    /// A new `Expr::Binary` instance.
    fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let operator = match operator.ttype {
            TokenKind::Minus => BinaryOpToken::Minus,
            TokenKind::Plus => BinaryOpToken::Plus,
            TokenKind::Star => BinaryOpToken::Star,
            TokenKind::Slash => BinaryOpToken::Slash,
            TokenKind::GreaterEqual => BinaryOpToken::GreaterEqual,
            TokenKind::Greater => BinaryOpToken::Greater,
            TokenKind::LessEqual => BinaryOpToken::LessEqual,
            TokenKind::Less => BinaryOpToken::Less,
            TokenKind::EqualEqual => BinaryOpToken::EqualEqual,
            TokenKind::BangEqual => BinaryOpToken::NotEqual,
            _ => panic!(
                "Parsing error found incorrect value as a binary operator {:?}.",
                operator.ttype
            ),
        };

        let b = Binary {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        };
        Self::Binary(b)
    }
    /// Creates a new unary expression.
    ///
    /// # Arguments
    ///
    /// * `operator` - The operator token.
    /// * `value` - The expression to apply the operator to.
    ///
    /// # Returns
    ///
    /// A new `Expr::Unary` instance.
    fn unary(operator: UnaryOpToken, value: Expr) -> Self {
        let u = Unary {
            operator,
            value: Box::new(value),
        };
        Self::Unary(u)
    }
    /// Creates a new assignment expression.
    ///
    /// # Arguments
    ///
    /// * `variable` - The variable token.
    /// * `value` - The expression to assign to the variable.
    ///
    /// # Returns
    ///
    /// A new `Expr::Assign` instance.
    fn assign(variable: Token, value: Expr) -> Self {
        let a = Assign {
            variable,
            value: Box::new(value),
        };
        Self::Assign(a)
    }
    /// Creates a new function call expression.
    ///
    /// # Arguments
    ///
    /// * `callee` - The expression representing the function to call.
    /// * `parentheses` - The parentheses token.
    /// * `arguments` - A vector of argument expressions.
    ///
    /// # Returns
    ///
    /// A new `Expr::Call` instance.
    fn call(callee: Expr, parentheses: Token, arguments: Vec<Expr>) -> Self {
        let c = Call {
            callee: Box::new(callee),
            parentheses,
            arguments,
        };
        Self::Call(c)
    }
    /// Creates a new property access expression.
    ///
    /// # Arguments
    ///
    /// * `object` - The expression representing the object.
    /// * `name` - The property name token.
    ///
    /// # Returns
    ///
    /// A new `Expr::Get` instance.
    fn get(object: Expr, name: Token) -> Self {
        let g = Get {
            object: Box::new(object),
            name,
        };
        Self::Get(g)
    }
    /// Creates a new grouping expression.
    ///
    /// # Arguments
    ///
    /// * `expression` - The expression inside the grouping.
    ///
    /// # Returns
    ///
    /// A new `Expr::Grouping` instance.
    fn grouping(expression: Expr) -> Self {
        let g = Grouping {
            expression: Box::new(expression),
        };
        Self::Grouping(g)
    }

    /// Creates a new literal expression.
    ///
    /// # Arguments
    ///
    /// * `value` - The literal value.
    ///
    /// # Returns
    ///
    /// A new `Expr::Literal` instance.
    fn literal(value: Object) -> Self {
        let l = Literal { value };
        Self::Literal(l)
    }
    /// Creates a new logical expression.
    ///
    /// # Arguments
    ///
    /// * `lhs` - The left-hand side expression.
    /// * `operator` - The operator token.
    /// * `rhs` - The right-hand side expression.
    ///
    /// # Returns
    ///
    /// A new `Expr::Logical` instance.
    fn logical(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let l = Logical {
            lhs: Box::new(lhs),
            operator,
            rhs: Box::new(rhs),
        };
        Self::Logical(l)
    }
    /// Creates a new property set expression.
    ///
    /// # Arguments
    ///
    /// * `object` - The expression representing the object.
    /// * `name` - The property name token.
    /// * `value` - The expression to set the property to.
    ///
    /// # Returns
    ///
    /// A new `Expr::Set` instance.
    fn set(object: Expr, name: Token, value: Expr) -> Self {
        let s = Set {
            object: Box::new(object),
            name,
            value: Box::new(value),
        };
        Self::Set(s)
    }

    /// Creates a new super expression.
    ///
    /// # Arguments
    ///
    /// * `keyword` - The 'super' keyword token.
    /// * `method` - The method name token.
    ///
    /// # Returns
    ///
    /// A new `Expr::Super` instance.
    fn super_expr(keyword: Token, method: Token) -> Self {
        let s = Super { keyword, method };
        Self::Super(s)
    }
    /// Creates a new 'this' expression.
    ///
    /// # Arguments
    ///
    /// * `keyword` - The 'this' keyword token.
    ///
    /// # Returns
    ///
    /// A new `Expr::This` instance.
    fn this(keyword: Token) -> Self {
        let t = This { keyword };
        Self::This(t)
    }
    /// Creates a new variable expression.
    ///
    /// # Arguments
    ///
    /// * `name` - The variable name token.
    ///
    /// # Returns
    ///
    /// A new `Expr::Variable` instance.
    fn variable(name: Token) -> Self {
        let v = Variable { name };
        Self::Variable(v)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: BinaryOpToken,
}

#[derive(Debug, Clone)]
pub enum BinaryOpToken {
    Plus,
    Minus,
    Star,
    Slash,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    EqualEqual,
    NotEqual,
}

impl Display for BinaryOpToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Plus => write!(f, "Plus"),
            Self::Minus => write!(f, "Minus"),
            Self::Star => write!(f, "Star"),
            Self::Slash => write!(f, "Slash"),
            Self::GreaterEqual => write!(f, "GreaterEqual"),
            Self::Greater => write!(f, "Greater"),
            Self::LessEqual => write!(f, "LessEqual"),
            Self::Less => write!(f, "Less"),
            Self::EqualEqual => write!(f, "EqualEqual"),
            Self::NotEqual => write!(f, "NotEqual"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub value: Box<Expr>,
    pub operator: UnaryOpToken,
}

#[derive(Debug, Clone)]
pub enum UnaryOpToken {
    Minus,
    Bang,
}

impl Display for UnaryOpToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Minus => write!(f, "Minus"),
            Self::Bang => write!(f, "Bang"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub variable: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expr>,
    pub parentheses: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub object: Box<Expr>,
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Object,
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub lhs: Box<Expr>,
    pub operator: Token,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub object: Box<Expr>,
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct Super {
    pub keyword: Token,
    pub method: Token,
}

#[derive(Debug, Clone)]
pub struct This {
    pub keyword: Token,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Token,
}

#[derive(Debug, Clone)]
pub enum Statement {}
#[derive(Debug, Clone)]
pub enum Object {
    True,
    False,
    Null,
    Integer(i64),
    Float(f64),
    String(String),
}
