use crate::{lexer::{Keyword, TokenKind}, parser::Parser, token::Token};



impl Parser {
    pub fn parse_expression(&mut self) -> Expr {
        self.parse_equality()
    }
    pub fn parse_equality(&mut self) -> Expr {
        let mut expression = self.parse_comparison();
        while self.peek().equality() {
            self.step();
            let rhs = self.parse_comparison();
            let operator = self.previous();
            expression = Expr::binary(expression, operator.clone(), rhs);
        }
        expression
    }
    pub fn parse_comparison(&mut self) -> Expr {
        let mut expression = self.parse_term();
        while self.peek().comparison() {
            self.step();
            let rhs = self.parse_term();
            let operator = self.previous();
            expression = Expr::binary(expression, operator.clone(), rhs)
        }
        expression
    }
    pub fn parse_term(&mut self) -> Expr {
        let mut expression = self.parse_factor();
        while self.peek().term() {
            self.step();
            let rhs = self.parse_factor();
            let operator = self.previous();
            expression = Expr::binary(expression, operator.clone(), rhs)
        }
        expression
    }
    pub fn parse_factor(&mut self) -> Expr {
        let mut expression = self.parse_unary();
        while self.peek().factor() {
            self.step();
            let rhs = self.parse_unary();
            let operator = self.previous();
            expression = Expr::binary(expression, operator.clone(), rhs)
        }
        expression
    }
    pub fn parse_unary(&mut self) -> Expr {
        while self.peek().unary() {
            self.step();
            let rhs = self.parse_unary();
            let operator = self.previous();
            return Expr::unary(operator.clone(), rhs)
        }
        return self.parse_primary()
    }
    pub fn parse_primary(&mut self) -> Expr {
        if let Some(kw) = self.token_type().keyword(){
            match kw {
                Keyword::True => return Expr::literal(Object::True),
                Keyword::False => return Expr::literal(Object::False),
                Keyword::Null => return Expr::literal(Object::Null),
                _ => panic!("Parsed primary contains an invalid keyword")
            }
        };
        if let Some(i) = self.token_type().integer() {
            return Expr::literal(Object::Integer(i))
        };

        if let Some(f) = self.token_type().float() {
            return Expr::literal(Object::Float(f))
        };

        if self.token_type() == &TokenKind::LeftParen {
            let expr = self.parse_expression();
            if !(self.consume().ttype == TokenKind::RightParen) {
                panic!("Expected a right parenthesis after an expression starting with a left parenthesis.")
            }
            return Expr::grouping(expr)
        };
        panic!("Invalid primary found")
    }
}

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
    fn binary(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let b = Binary{lhs: Box::new(lhs), operator, rhs: Box::new(rhs)};
        Self::Binary(b)
    }
    fn unary(operator: Token, value: Expr) -> Self {
        let u = Unary{operator, value: Box::new(value)};
        Self::Unary(u)
    }

    fn assign(variable: Token, value: Expr) -> Self {
        let a = Assign{variable, value: Box::new(value)};
        Self::Assign(a)
    }

    fn call(callee: Expr, parentheses: Token, arguments: Vec<Expr>) -> Self {
        let c = Call{callee: Box::new(callee), parentheses, arguments};
        Self::Call(c)
    }

    fn get(object: Expr, name: Token) -> Self {
        let g = Get{object: Box::new(object), name};
        Self::Get(g)
    }

    fn grouping(expression: Expr) -> Self {
        let g = Grouping{expression: Box::new(expression)};
        Self::Grouping(g)
    }

    fn literal(value: Object) -> Self {
        let l = Literal{value};
        Self::Literal(l)
    }

    fn logical(lhs: Expr, operator: Token, rhs: Expr) -> Self {
        let l = Logical{lhs: Box::new(lhs), operator, rhs: Box::new(rhs)};
        Self::Logical(l)
    }

    fn set(object: Expr, name: Token, value: Expr) -> Self {
        let s = Set{object: Box::new(object), name, value: Box::new(value)};
        Self::Set(s)
    }

    fn super_expr(keyword: Token, method: Token) -> Self {
        let s = Super{keyword, method};
        Self::Super(s)
    }

    fn this(keyword: Token) -> Self {
        let t = This{keyword};
        Self::This(t)
    }

    fn variable(name: Token) -> Self {
        let v = Variable{name};
        Self::Variable(v)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub operator: Token,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub value: Box<Expr>,
    pub operator: Token,
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
#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
