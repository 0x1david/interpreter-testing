mod ast;
mod expression;
mod interpreter;
mod lexer;
mod parser;
mod statement;
mod token;

use lexer::Lexer;
use parser::Parser;
use interpreter::Interpreter;
use std::fs;
use std::path::PathBuf;

const FILE_PATH: &str = "./test_file.lngg";

fn main() {
    let script = fs::read_to_string(FILE_PATH).expect("Shouldnt fail reading this test file.");
    let mut lexer = Lexer::new(&script); 
    lexer.scan_tokens();
    println!("{:?}", lexer.tokens);
    let mut parser = Parser::new(lexer);
    println!("{:?}", parser.parse());
}
