
struct Block {
    statements: Vec<Statement>
}

struct Struct {
    name: Token,
    superclass: Variable,
    body: Block

}
