#![allow(dead_code)]
#![allow(unused)]

mod ast;
mod expression;
mod interpreter;
mod lexer;
mod parser;
mod statement;
mod token;
mod environment;

use lexer::Lexer;
use parser::Parser;
use std::fs;

use crate::interpreter::Interpreter;

const FILE_PATH: &str = "./test_file.lngg";

fn main() {
    // let script = fs::read_to_string(FILE_PATH).expect("Shouldnt fail reading this test file.");
    // let mut lexer = Lexer::new(&script);
    let mut lexer = Lexer::new("
    let x = 362 + 439 * 88 + 8 * 9 == 39066;
    print x;
                               ");
    lexer.scan_tokens();
    println!("{:?}", lexer.tokens);
    let mut parser = Parser::new(lexer);
    println!("{:?}", parser.parse());
}
