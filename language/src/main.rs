mod ast;
mod expression;
mod interpreter;
mod lexer;
mod parser;
mod statement;
mod token;

use lexer::Lexer;
use parser::Parser;
use std::fs;

use crate::interpreter::Interpreter;

const FILE_PATH: &str = "./test_file.lngg";

fn main() {
    // let script = fs::read_to_string(FILE_PATH).expect("Shouldnt fail reading this test file.");
    // let mut lexer = Lexer::new(&script);
    let mut lexer = Lexer::new("print 362 + 439 * 88 + 8 * 9 == 39066;");
    lexer.scan_tokens();
    println!("{:?}", lexer.tokens);
    let mut parser = Parser::new(lexer);
    println!("{:?}", parser.parse());
}
