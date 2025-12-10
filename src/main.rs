mod scanner;
mod token;
mod ast;
mod parser;

fn main() {
    let input = "fn foo() { 123 }";
    let mut parser = parser::Parser::new(input);
    println!("Tokens: {:?}", parser.tokens);
    let parsed = parser.parse();
    println!("Parsed: {parsed:?}");
}
