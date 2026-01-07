use std::fs;

mod scanner;
mod token;
mod ast;
mod parser;
mod s_expr;

fn main() {
    let input_bytes = fs::read("program.eris").unwrap();
    let input = String::from_utf8(input_bytes).unwrap();
    let mut parser = parser::Parser::new(&input);
    println!("Tokens: {:#?}", parser.tokens);
    let parsed = parser.parse();
    println!("Parsed: {parsed:#?}");
}
