use std::fmt::Display;

use crate::{
    lexer::{Keyword, TokenKind},
    parser::Parser,
    statement::{Expression, Statement},
    token::Token,
};

impl Parser {
    /// Parses the current expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed expression.
    pub fn parse_expression(&mut self) -> Expr {
        self.parse_assignment()
            .expect("After finishing, expression should be parsed")
    }
 
    /// Parses the assignment expression or any lower priority exression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed expression.
    pub fn parse_assignment(&mut self) -> Option<Expr> {
        let expression = self.parse_and();
        if self.peek().ttype.equal() {
            self.step();
            let var = self.previous();
            let value = Box::new(self.parse_assignment().expect("Assignemnt rhs expects an expression."));
            if let Some(Expr::Variable(ref v)) = expression {
                let name = &v.name;
                return Some(Expr::Assign(Assign {variable: Token::new(TokenKind::Identifier(name.to_string()), None, 9999999), value}));
            } else {
                panic!("Invalid assignment target. Got: {:?}", expression);
            };
        }
        expression
    }

    /// Parses the logical statement AND or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed and expression.
    pub fn parse_and(&mut self) -> Option<Expr> {
        let mut expression = self.parse_or()?;

        while self.peek().and() {
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_or()?;
            println!("The operator at equality is {:?}", operator.ttype);
            expression = Expr::logical(expression, operator, rhs);
        }
        Some(expression)
    }

    /// Parses the logical statement OR, or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed OR expression.
    pub fn parse_or(&mut self) -> Option<Expr> {
        let mut expression = self.parse_equality()?;
        while self.peek().or() {
            self.step();
            let operator = self.previous().clone();
            let rhs = self.parse_equality()?;
            println!("The operator at equality is {:?}", operator.ttype);
            expression = Expr::logical(expression, operator, rhs);
        }
        Some(expression)
    }

    /// Parses equality expressions (`==`, `!=`) or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed equality expression.
    pub fn parse_equality(&mut self) -> Option<Expr> {
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
        let mut expression = self.parse_factor()?;
        while self.peek().term() {
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
        self.parse_func_call()
    }

    /// Parses a function call or any lower priority expression.
    ///
    /// # Returns
    /// An `Expr` representing the parsed unary expression.
    pub fn parse_func_call(&mut self) -> Option<Expr> {
        let mut expr = self.parse_primary();

        loop {
            if self.peek().ttype.left_paren()  {
                expr = Some(self.finish_call(expr?))
            } else {
                break
            }
        };

        expr
    }
    
    pub fn finish_call(&mut self, callee: Expr) -> Expr {
        let mut args = vec![];

        self.step();

        if !self.peek().ttype.right_paren() {
            loop {
                args.push(self.parse_expression());
                if !self.peek().ttype.comma() {
                    break
                }
                // self.step() // TODO IS THIS STEP NEEDED?
            };
        }
        if !(self.consume().ttype == TokenKind::RightParen) {
            panic!("Expected a right parenthesis after the arguments of a function call, got: {:?}.", &self.peek().ttype)
        }
        if args.len() > 255 {
            panic!("A function call can't have more than 255 arguments.")
        }
        Expr::call(callee, self.consume().clone(), args)
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
            self.step();
            return Some(Expr::literal(Object::Integer(i)));
        };

        if let Some(f) = self.token_type().float() {
            self.step();
            return Some(Expr::literal(Object::Float(f)));
        };

        if let Some(ident) = self.token_type().identifier() {
            let ident_copy = ident.to_string();
            self.step();
            return Some(Expr::variable(ident_copy));
        };
        if let Some(s) = self.token_type().string() {
            let s_copy = s.to_string();
            self.step();
            return Some(Expr::literal(Object::String(s_copy)));
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

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Variable(val) => write!(f, "{}", val),
            Expr::This(val) => write!(f, "{}", val),
            Expr::Super(val) => write!(f, "{}", val),
            Expr::Set(val) => write!(f, "{}", val),
            _ => write!(f, "Cannot be printed (as of now...)"),
        }
    }
}

impl Expr {
    pub fn len(&self) -> Option<usize> {
        match self {
            Expr::Variable(val) => Some(val.name.len()),
            Expr::Literal(Literal { value }) => Some(value.len()),
            _ => None,
        }
    }

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

    /// Converts an expression to an Expression struct (used in expression stmts).
    ///
    /// # Returns
    ///
    /// A new `Statement::Expr::Logical` instance.
    pub fn wrap_expression(self) -> Expression {
        Expression { expression: self }
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
    fn variable(name: String) -> Self {
        let v = Variable {
            name: name.to_string(),
            initializer: None
        };
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

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
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

impl Display for Set {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        panic!("Can't call print on a SET Expr")
    }
}

#[derive(Debug, Clone)]
pub struct Super {
    pub keyword: Token,
    pub method: Token,
}

impl Display for Super {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.keyword)
    }
}

#[derive(Debug, Clone)]
pub struct This {
    pub keyword: Token,
}

impl Display for This {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.keyword)
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: String,
    pub initializer: Option<Box<Expr>>
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Object {
    True,
    False,
    Null,
    Integer(i64),
    Float(f64),
    String(String),
}

impl Object {
    pub fn len(&self) -> usize {
        match self {
            Object::String(s) => s.len(),
            _ => panic!("Type has no len"),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::True => write!(f, "True"),
            Self::False => write!(f, "False"),
            Self::String(s) => write!(f, "{}", s),
            Self::Float(n) => write!(f, "{}", n),
            Self::Integer(n) => write!(f, "{}", n),
            Self::Null => write!(f, "Null"),
        }
    }
}
