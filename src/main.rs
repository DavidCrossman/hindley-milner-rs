pub mod built_in;
pub mod environment;
pub mod expression;
pub mod free_variable;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod program;
pub mod type_checking;

fn main() {
    let source = r"
        Nat = zero + succ Nat
        main = succ (succ zero)
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(items) => match program::Program::new(items) {
                Ok(program) => match program.type_check() {
                    Ok(_) => match program.run() {
                        Ok(v) => println!("{v}"),
                        Err(e) => println!("runtime error: {:?}", anyhow::Error::from(e)),
                    },
                    Err(e) => println!("type error: {e}"),
                },
                Err(e) => println!("program error: {e}"),
            },
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
