use chumsky::Parser;

mod lexer;
mod parser;

fn main() {
    let result = lexer::lexer().parse("let x = y (z 5) in λp r -> r p x");
    println!("{result:?}");

    if let Ok(tokens) = result {
        let result = parser::parser().parse(tokens);
        println!("{result:?}");
    }
}
