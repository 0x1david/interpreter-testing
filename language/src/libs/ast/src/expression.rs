
#[derive(Debug, Clone)]
pub enum Expression{
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

#[derive(Debug, Clone)]
pub struct Binary {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub kind: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Unary {
    pub value: Box<Expression>,
    pub operator: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub variable: Token,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Call {
    pub callee: Box<Expression>,
    pub parentheses: Token,
    pub arguments: Vec<Box<Expression>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Get {
    pub object: Box<Expression>,
    pub name: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Grouping {
    pub expression: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Literal {
    pub value: Object,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Logical {
    pub lhs: Box<Expression>,
    pub operator: Token,
    pub rhs: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Set {
    pub object: Box<Expression>,
    pub name: Token,
    pub value: Box<Expression>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Super {
    pub keyword: Token,
    pub method: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct This {
    pub keyword: Token,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Token,
    pub span: Span,
}



#[derive(Debug, Clone)]
pub enum Statement{}
#[derive(Debug, Clone)]
pub enum Token{}
#[derive(Debug, Clone)]
pub enum Object{}
#[derive(Debug, Clone)]
pub struct Span{
    pub start: usize,
    pub end: usize,
}
