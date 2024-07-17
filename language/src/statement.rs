use crate::expression::{Binary, BinaryOpToken, Expr, Literal, Object, Variable};
use crate::lexer::{Keyword, TokenKind};
use crate::parser::Parser;
use crate::token::Token;

/// Represents different kinds of statements in the language.
#[derive(Debug, Clone)]
pub enum Statement {
    Block(Block),
    Struct(Struct),
    Expression(Expression),
    Procedure(Procedure),
    If(If),
    Print(Print),
    Return(Return),
    Let(Let),
    While(While),
    Variable(Variable),
}

impl Statement {

    pub fn get_inner_expr(self) -> Expr {
        match self {
            Self::Expression(e) => e.expression,
            _ => panic!("Doesn't contain inner expression.")
        }
    }
    pub fn get_inner_block(&self) -> Vec<Statement>{
        match self {
            Self::Block(b) => b.statements.clone(),
            _ => panic!("Isn't a block .")
        }
    }
}

impl Parser {

    // Manufacture a while condition
    pub fn manufacture_while(&self, condition: Expression, body: Box<Statement>) -> Statement {
        Statement::While(While { condition, body })
    }

    /// Parses a statement and returns an optional `Statement`.
    pub fn parse_statement(&mut self) -> Option<Statement> {
        let tokenkind = &self.consume().ttype.clone();
        let kw = tokenkind.keyword();

        if let Some(kw) = kw {
            match kw {
                Keyword::Let => self.parse_let(),
                Keyword::Print => self.parse_print(),
                Keyword::If => self.parse_if(),
                Keyword::While => self.parse_while(),
                Keyword::For => self.parse_for(),
                Keyword::Proc => self.parse_proc(),
                _ => unimplemented!(),
            }
        } else if let Some(ident) = tokenkind.get_identifier() {
            self.parse_variable_declaration(ident.to_string())
        } else if tokenkind.left_brace() {
            self.parse_block()
        } else {
            self.parse_expression_stmt()
        }
    }

    /// Parses a print statement and returns an optional `Statement`.
    fn parse_print(&mut self) -> Option<Statement> {
        let value = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Print(Print { expression: value }))
    }

    /// Parses a while loop and returns an optional `Statement`.
    fn parse_while(&mut self) -> Option<Statement> {;
        let condition = self.parse_expression().wrap_expression();
        let body = Box::new(
            self.parse_statement()
                .expect("Expected a block after the condition of a while keyword"),
        );

        Some(Statement::While(While { condition, body }))
    }

    /// Parses a while loop and returns an optional `Statement`.
    /// For x in items {
    ///     EXECUTION BLOCK
    /// }
    /// For (n = 0; n < 10; n+=1)
    ///
    ///
    /// while n < items.length {
    ///     let x = items[n]
    ///     EXECUTION_BLOCK
    ///     n += 1
    /// }
    fn parse_for(&mut self) -> Option<Statement> {

        if !self.consume().ttype.left_paren() {
            panic!("Incorrect form of calling a for loop, expected '(' after the FOR keyword variable name.");
        }

        let initializer = if self.peek().ttype.semicolon() {
            None
        } else if self.peek().ttype.keyword() == Some(&Keyword::Let) {
            self.step();
            self.parse_let()
        } else {
            self.parse_expression_stmt()
        };
        println!("INITIALIZER IS: {:?}", &initializer);

        let condition = if self.peek().ttype.semicolon() {
            Expr::Literal(Literal {
                value: Object::True,
            })
        } else {
            self.parse_expression()
        };
        if !self.consume().ttype.semicolon() {
            panic!("Incorrect form of calling a for loop, expected a ';' after the condition of a FOR loop.");
        }
        println!("CONDITION IS: {:?}", &condition);

        let increment: Option<Expr>;
        let increment = if self.peek().ttype.right_paren() {
            None
        } else {
            Some(self.parse_expression())
        };
        if !self.consume().ttype.right_paren() {
            panic!("Incorrect form of calling a for loop, expected a ')' before the block of a FOR loop.");
        }
        println!("INCREMENT IS: {:?}", &increment);

        self.step();
        let mut body = vec![self
            .parse_block()
            .expect("Expected a block to follow a FOR loop.")];

        let mut statements = vec![];

        if let Some(init) = initializer {
            statements.push(init)
        };

        if let Some(increment) = increment {
            body.push(Statement::Expression(Expression {
                expression: increment,
            }))
        };
        let body = Box::new(Statement::Block(Block { statements: body }));

        statements.push(Statement::While(While { condition: Expression { expression: condition, }, body, }));

        Some(Statement::Block(Block { statements }))
    }

    fn parse_function(&mut self) {

    }

    /// Parses a print statement and returns an optional `Statement`.
    fn parse_if(&mut self) -> Option<Statement> {
        let cond = self.parse_expression();
        if !self.consume().ttype.left_brace() {
            panic!("Expected left brace after stating the condition of an if statement, proper error handling is TBD.")
        };
        let block = self.parse_block()?;
        let mut elifs = vec![];
        while self.peek().ttype == TokenKind::Keyword(Keyword::Elif) {
            self.step();
            let elif_cond = self.parse_expression();
            if !self.consume().ttype.left_brace() {
                panic!("Expected left brace after stating the condition of an if statement, proper error handling is TBD.")
            };
            let elif_block = self.parse_block()?;
            elifs.push((elif_cond, elif_block))
        }
        if self.consume().ttype == TokenKind::Keyword(Keyword::Else) {
            if !self.consume().ttype.left_brace() {
                panic!("Expected left brace after stating the condition of an if statement, proper error handling is TBD.")
            };
            let else_block = self.parse_block()?;
            Some(Statement::If(If {
                condition: cond,
                then_branch: Box::new(block),
                elif_branches: elifs,
                else_branch: Some(Box::new(else_block)),
            }))
        } else {
            Some(Statement::If(If {
                condition: cond,
                then_branch: Box::new(block),
                elif_branches: elifs,
                else_branch: None,
            }))
        }
    }

    /// Parses a let statement and returns an optional `Statement`.
    fn parse_let(&mut self) -> Option<Statement> {
        let identifier = self.consume().clone();
        println!("{:?}", identifier.ttype);

        let value = if self.consume().ttype == TokenKind::Equal {
            self.parse_expression()
        } else {
            panic!("Expected an equal during an assignment parsing after let. Got: {:?}", self.previous().ttype)
        };

        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after let statement, proper error handling is TBD.")
        };

        Some(Statement::Let(Let {
            name: identifier,
            initializer: value,
        }))
    }

    /// Parser procedure definition and returns an optional statement
    fn parse_proc(&mut self) -> Option<Statement> {
        let mut params = vec![];
        let name = self.consume().clone();
        if !name.identifier() {
            panic!("Function name should be an identifier.");
        }
        if !self.consume().ttype.left_paren() {
            panic!("Expected a left parenthesis after the function identifier in a definition");
        }

        let mut param_name;
        if !self.consume().ttype.right_paren() {
            loop {
                if params.len() >= 255 {
                    panic!("Can't have more than 255 parameters.")
                }
                param_name = self.consume();

                if !param_name.ttype.identifier() {
                    panic!("Expected parameter name.")
                }
                params.push(param_name.clone());

                if self.peek().ttype.right_paren() {
                    self.consume();
                    break;
                }

                if !self.consume().ttype.comma() {
                    panic!("Expected ',' or ')' after parameter.");
                }
                    }
                }
        if ! self.consume().ttype.left_brace() {
            let e = r#"Expected '{' before a function body"#;
            panic!("{}", e);
        }
        let body = self.parse_block().expect("Expected whole body definition in function declaration").get_inner_block();
        
        Some(Statement::Procedure(Procedure {
            name: name.clone(), 
            params,
            body
        }))
    }

    /// Parses a block and returns an optional `Statement`.
    fn parse_block(&mut self) -> Option<Statement> {
        let mut stmts = vec![];
        while !self.peek().ttype.right_brace() && !self.is_at_end() {
            stmts.push(
                self.parse_statement()
                    .expect("Found something else than a statement in a block."),
            );
        }


        if self.peek().ttype.right_brace() {
            self.consume();
            Some(Statement::Block(Block { statements: stmts }))
        } else {
            panic!("Expected right brace. Got: {:?}", self.peek().ttype)
        }
    }
    /// Parses an expression statement and returns an optional `Statement`.
    fn parse_expression_stmt(&mut self) -> Option<Statement> {
        let expr = self.parse_expression();
        if self.consume().ttype != TokenKind::Semicolon {
            panic!("Expected semicolon after expression statement, proper error handling is TBD.")
        };
        Some(Statement::Expression(Expression { expression: expr }))
    }

    /// Parses a variable expression and returns an optional `Statement`.
    fn parse_variable_declaration(&mut self, ident: String) -> Option<Statement> {
        println!("IN VARIABLE = {:?}", self.peek());
        let initializer = if self.peek().ttype.equal() {
            self.step();
            Some(Box::new(self.parse_expression_stmt().expect("rhs of an assignment should always be an expression").get_inner_expr()))
        } else {
            None
        };

        Some(Statement::Variable(Variable {
            name: ident.to_string(),
            initializer
        }))
    }
}

/// Represents a block of statements.
#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

/// Represents a struct declaration.
#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Token,
    pub superclass: Variable,
    pub body: Block,
}

/// Represents an expression statement.
#[derive(Debug, Clone)]
pub struct Expression {
    pub expression: Expr,
}
impl Expression {
    pub fn manufacture_binary(lhs: Expr, rhs: Expr, operator: BinaryOpToken) -> Expression {
        let lhs = Box::new(lhs);
        let rhs = Box::new(rhs);
        Expression {
            expression: Expr::Binary(Binary { lhs, operator, rhs }),
        }
    }
    pub fn manufacture_variable(name: String) -> Expression {
        Expression {
            expression: Expr::Variable(Variable { name, initializer: None }),
        }
    }

    pub fn manufacture_literal(i: i64) -> Expression {
        Expression {
            expression: Expr::Literal(Literal {
                value: Object::Integer(i),
            }),
        }
    }
}

/// Represents a procedure declaration.
#[derive(Debug, Clone)]
pub struct Procedure {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

/// Represents a function declaration.
#[derive(Debug, Clone)]
pub struct Function {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>,
}

type ElifNode = (Expr, Statement);
type ElseNode = Option<Box<Statement>>;

/// Represents an if statement.
#[derive(Debug, Clone)]
pub struct If {
    pub condition: Expr,
    pub then_branch: Box<Statement>,
    pub elif_branches: Vec<ElifNode>,
    pub else_branch: ElseNode,
}

/// Represents a print statement.
#[derive(Debug, Clone)]
pub struct Print {
    pub expression: Expr,
}

/// Represents a return statement.
#[derive(Debug, Clone)]
pub struct Return {
    pub keyword: Token,
    pub value: Expr,
}

/// Represents a let statement.
#[derive(Debug, Clone)]
pub struct Let {
    pub name: Token,
    pub initializer: Expr,
}

/// Represents a while loop statement.
#[derive(Debug, Clone)]
pub struct While {
    pub condition: Expression,
    pub body: Box<Statement>,
}
