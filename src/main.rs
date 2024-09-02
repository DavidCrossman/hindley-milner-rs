pub mod expression;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_checking;

fn main() {
    let source = r"
        id x = x; const x y = x;
        flip f x y = f y x
        main = flip const (id 0) (id true)
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(program) => match type_checking::type_check(&program) {
                Ok(_) => match interpreter::run(&program) {
                    Ok(e) => println!("{e}"),
                    Err(e) => println!("runtime error: {e}"),
                },
                Err(e) => println!("type error: {e}"),
            },
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
