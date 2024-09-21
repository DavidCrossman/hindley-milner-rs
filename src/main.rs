pub mod interpreter;
pub mod kind_inference;
pub mod lexer;
pub mod model;
pub mod parser;
pub mod program;
pub mod type_inference;

fn main() {
    let source = r"
        type List a = nil + cons a (List a)
        main = cons 3 (cons 2 nil)
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(items) => match program::Program::new(items) {
                Ok(mut program) => match program.type_check() {
                    Ok(_) => match program.kind_check() {
                        Ok(_) => match program.run() {
                            Ok(v) => println!("{v}"),
                            Err(e) => println!("runtime error: {:?}", anyhow::Error::from(e)),
                        },
                        Err(e) => println!("kind error: {e}"),
                    },
                    Err(e) => println!("type error: {e}"),
                },
                Err(e) => println!("program error: {:?}", anyhow::Error::from(e)),
            },
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
