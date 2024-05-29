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
        self.parse_equality().expect("After finishing, expression should be parsed")
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
            let operator = self.previous().clone();
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
                    return Some(Expr::literal(Object::False))
                }
                Keyword::Null => {
                    self.step();
                    return Some(Expr::literal(Object::Null))
                }
                _ => return None 
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
    fn unary(operator: Token, value: Expr) -> Self {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::TokenKind;
    use crate::parser::Parser;
    use crate::token::Span;

    fn token(ttype: TokenKind, line: usize, start: usize, end: usize) -> Token {
        Token {
            ttype,
            literal: None,
            line,
            span: Span::new(start, end),
        }
    }

    #[test]
    fn test_simple_expression() {
        // 1 + 3 * ( 8 + 5 ) == 48
        //
        dbg!("running test");
        let tokens = vec![
            token(TokenKind::Integer(1), 1, 1, 2),
            token(TokenKind::Plus, 1, 1, 2),
            token(TokenKind::Integer(3), 1, 1, 2),
            token(TokenKind::Star, 1, 1, 2),
            token(TokenKind::LeftParen, 1, 1, 2),
            token(TokenKind::Integer(8), 1, 1, 2),
            token(TokenKind::Plus, 1, 1, 2),
            token(TokenKind::Integer(5), 1, 1, 2),
            token(TokenKind::RightParen, 1, 1, 2),
            token(TokenKind::EqualEqual, 1, 1, 2),
            token(TokenKind::Integer(48), 1, 1, 2),
            token(TokenKind::EOF, 1, 1, 2),
        ];

        dbg!("Init parser");
        let mut parser = Parser::from_token_vec(tokens);
        dbg!("Running parse");
        parser.parse();
        dbg!("Ending parse");

        assert_eq!(parser.tokens.len(), 1, "Expected 8 top-level statements");
    }

    // #[test]
    // fn test_parse_complex_source_code() {
    //     let expected_tokens = vec![
    //         token(TokenKind::Keyword(Keyword::Let), 1, 0, 3),
    //         token(TokenKind::Identifier("x".to_string()), 1, 4, 5),
    //         token(TokenKind::Equal, 1, 6, 7),
    //         token(TokenKind::Integer(42), 1, 8, 10),
    //         token(TokenKind::Semicolon, 1, 10, 11),
    //         token(TokenKind::Keyword(Keyword::Let), 2, 0, 3),
    //         token(TokenKind::Identifier("y".to_string()), 2, 4, 5),
    //         token(TokenKind::Equal, 2, 6, 7),
    //         token(TokenKind::Float(3.22), 2, 8, 12),
    //         token(TokenKind::Semicolon, 2, 12, 13),
    //         token(TokenKind::Keyword(Keyword::Let), 3, 0, 3),
    //         token(TokenKind::Identifier("message".to_string()), 3, 4, 11),
    //         token(TokenKind::Equal, 3, 12, 13),
    //         token(TokenKind::String("Hello, World!".to_string()), 3, 14, 29),
    //         token(TokenKind::Semicolon, 3, 29, 30),
    //         token(TokenKind::Keyword(Keyword::Let), 4, 0, 3),
    //         token(TokenKind::Identifier("char_literal".to_string()), 4, 4, 16),
    //         token(TokenKind::Equal, 4, 17, 18),
    //         token(TokenKind::String("c".to_string()), 4, 19, 22),
    //         token(TokenKind::Semicolon, 4, 22, 23),
    //         token(TokenKind::Keyword(Keyword::Const), 5, 0, 5),
    //         token(TokenKind::Identifier("MAX".to_string()), 5, 6, 9),
    //         token(TokenKind::Equal, 5, 10, 11),
    //         token(TokenKind::Integer(100), 5, 12, 15),
    //         token(TokenKind::Semicolon, 5, 15, 16),
    //         token(TokenKind::Keyword(Keyword::If), 6, 0, 2),
    //         token(TokenKind::Identifier("x".to_string()), 6, 3, 4),
    //         token(TokenKind::Greater, 6, 5, 6),
    //         token(TokenKind::Integer(10), 6, 7, 9),
    //         token(TokenKind::LeftBrace, 6, 10, 11),
    //         token(TokenKind::Identifier("println".to_string()), 7, 4, 11),
    //         token(TokenKind::LeftParen, 7, 11, 12),
    //         token(
    //             TokenKind::String("x is greater than 10".to_string()),
    //             7,
    //             12,
    //             34,
    //         ),
    //         token(TokenKind::RightParen, 7, 34, 35),
    //         token(TokenKind::Semicolon, 7, 35, 36),
    //         token(TokenKind::RightBrace, 8, 0, 1),
    //         token(TokenKind::Keyword(Keyword::Else), 9, 0, 4),
    //         token(TokenKind::LeftBrace, 9, 5, 6),
    //         token(TokenKind::Identifier("println".to_string()), 10, 4, 11),
    //         token(TokenKind::LeftParen, 10, 11, 12),
    //         token(TokenKind::String("x is 10 or less".to_string()), 10, 12, 29),
    //         token(TokenKind::RightParen, 10, 29, 30),
    //         token(TokenKind::Semicolon, 10, 30, 31),
    //         token(TokenKind::RightBrace, 11, 0, 1),
    //         token(TokenKind::Keyword(Keyword::While), 12, 0, 5),
    //         token(TokenKind::Identifier("y".to_string()), 12, 6, 7),
    //         token(TokenKind::Less, 12, 8, 9),
    //         token(TokenKind::Identifier("MAX".to_string()), 12, 10, 13),
    //         token(TokenKind::LeftBrace, 12, 14, 15),
    //         token(TokenKind::Identifier("y".to_string()), 13, 4, 5),
    //         token(TokenKind::Equal, 13, 6, 7),
    //         token(TokenKind::Identifier("y".to_string()), 13, 8, 9),
    //         token(TokenKind::Plus, 13, 10, 11),
    //         token(TokenKind::Integer(1), 13, 12, 13),
    //         token(TokenKind::Semicolon, 13, 13, 14),
    //         token(TokenKind::RightBrace, 14, 0, 1),
    //         token(TokenKind::EOF, 14, 1, 1),
    //     ];

    //     let mut parser = Parser::from_token_vec(expected_tokens);
    //     parser.parse();

    //     dbg!("{:?}", parser.tokens);
    //     assert_eq!(parser.tokens.len(), 8, "Expected 8 top-level statements");
    // }
}
