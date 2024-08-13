use chumsky::Parser;

mod lexer;
mod parser;

fn main() {
    let result = lexer::lexer().parse("let x = x (y 5) z in \\p -> p x");
    println!("{result:?}");

    if let Ok(tokens) = result {
        let result = parser::parser().parse(tokens);
        println!("{result:?}");
    }
}
