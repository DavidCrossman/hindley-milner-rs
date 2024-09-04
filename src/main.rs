pub mod environment;
pub mod expression;
pub mod free_variable;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_checking;

use environment::Environment;
use parser::Item;

fn main() {
    let source = r"
    id : a → a
    const x _ = id x
    id x = x
    flip : (a → b → c) → b → a → c
    flip f x y = f y x
    main : Bool
    main = flip const (id 0) (id true)
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(items) => {
                let mut program = Vec::new();
                let mut env = Environment::new();
                items.into_iter().for_each(|item| match item {
                    Item::Definition(name, expr) => program.push((name, expr)),
                    Item::Declaration(name, m) => env += (name, m.generalise(&env)),
                });
                match type_checking::type_check(&program, env) {
                    Ok(_) => match interpreter::run(&program) {
                        Ok(e) => println!("{e}"),
                        Err(e) => println!("runtime error: {e}"),
                    },
                    Err(e) => println!("type error: {e}"),
                }
            }
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
