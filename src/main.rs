mod scanner;
mod token;
mod ast;
mod parser;

fn main() {
    let input = "let number12 = 0b10_10 + 456";
    let mut parser = parser::Parser::new(input);
    let parsed = parser.parse();
}
