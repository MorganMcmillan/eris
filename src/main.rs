mod token;
mod scanner;

fn main() {
    let tokens = token::Token::from_input("let a = 1 + 2");
    println!("{tokens:?}");
    
    let mut scanner = scanner::Scanner::new("Hello world!");
    while let Some(c) = scanner.next() {
        println!("Scanned: {c}");
    }
}
