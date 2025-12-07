mod token;
mod scanner;
mod ast;
mod parser;

fn main() {
    let input = "let number = 0b10_10 + 456";
    let tokens = token::Token::from_input(input);
    for token in tokens.iter() {
        let span = token.span(input);
        println!("Token: {:?}, Lexeme: \"{}\", span: {span:?}", token.token_type, token.lexeme);
    }
}
