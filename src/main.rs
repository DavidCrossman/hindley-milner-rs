use chumsky::Parser;

mod lexer;
mod model;
mod parser;
mod substitution;
mod unification;
mod variable;

fn main() {
    let result = lexer::lexer().parse("let x = y (z 5) in λp r -> r p x");
    println!("{result:?}");

    if let Ok(tokens) = result {
        let result = parser::parser().parse(tokens);
        match result {
            Ok(e) => println!("{e}"),
            Err(e) => println!("{e:?}"),
        }
    }
}
