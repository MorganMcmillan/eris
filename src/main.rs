use std::fs;

use logos::Logos;

use crate::token::Token;

mod token;
mod ast;
mod parser;
mod s_expr;

fn main() {
    let input_bytes = fs::read("program.eris").unwrap();
    let input = String::from_utf8(input_bytes).unwrap();
    let mut lexer = Token::lexer(&input);
    let mut parser = parser::Parser::new(lexer);
    let parsed = parser.parse();
    println!("Parsed: {parsed:#?}");
}
