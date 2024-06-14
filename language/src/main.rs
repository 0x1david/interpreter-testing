#![allow(dead_code)]
#![allow(unused)]

mod environment;
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
    // let mut lexer = Lexer::new(
    // "
    // let x = 362 + 439 * 88 + 8 * 9 == 39066;
    // print x;
    // ",
    // );
    let mut lexer = Lexer::new(
        r#"
        //let n = 0;
        for (let x = 0; x < 4; x = x + 1) {
            print x;
        }
        // let strx = "apple";
        // if strx == "apple" and False{
        //     print "if branch";
        // } elif strx == "pineapple" {
        //     print "first elif branch";
        // } elif strx == "pear" {
        //     print "second elif branch";
        // } else {
        //     print "else branch";
        // }
        // let foo = 1;
        // let bar = 2;
        // print strx;
        // {
        //     print foo;
        //     let bar = 4;
        //     print bar;
        //     {
        //         let baz = 893;
        //     }
        //     print bar;
        // }
        // print foo;
        "#,
    );
    lexer.scan_tokens();
    println!("{:?}", lexer.tokens);
    let mut parser = Parser::new(lexer);
    println!("{:?}", parser.parse());
}
