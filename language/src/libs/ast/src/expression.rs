
#[derive(Debug, Clone)]
enum Expression{
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
struct Binary {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    kind: Token,
}

#[derive(Debug, Clone)]
struct Unary {
    value: Box<Expression>,
    operator: Token,
}

#[derive(Debug, Clone)]
struct Assign {
    variable: Token,
    value: Box<Expression>
}

#[derive(Debug, Clone)]
struct Call {
    callee: Box<Expression>,
    parentheses: Token,
    arguments: Vec<Box<Expression>>,
}

#[derive(Debug, Clone)]
struct Get {
    object: Box<Expression>,
    name: Token,
}

#[derive(Debug, Clone)]
struct Grouping {
    expression: Box<Expression>,
}

#[derive(Debug, Clone)]
struct Literal {
    value: Object,
}

#[derive(Debug, Clone)]
struct Logical {
    lhs: Box<Expression>,
    operator: Token,
    rhs: Box<Expression>
}

#[derive(Debug, Clone)]
struct Set {
    object: Box<Expression>,
    name: Token,
    value: Box<Expression>,
}

#[derive(Debug, Clone)]
struct Super {
    keyword: Token,
    method: Token,
}

#[derive(Debug, Clone)]
struct This {
    keyword: Token
}

#[derive(Debug, Clone)]
struct Variable {
    name: Token
}



#[derive(Debug, Clone)]
enum Statement{}
#[derive(Debug, Clone)]
enum Token{}
#[derive(Debug, Clone)]
enum Object{}
