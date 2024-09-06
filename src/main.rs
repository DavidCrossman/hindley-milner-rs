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
        add : Int -> Int -> Int
        add = builtin
        main = add 5 7
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(items) => match program::Program::new(items) {
                Ok(program) => match program.type_check() {
                    Ok(_) => match program.run() {
                        Ok(e) => println!("{e}"),
                        Err(e) => println!("runtime error: {e}"),
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
