mod algorithm;
mod interpreter;
mod lexer;
mod model;
mod parser;
mod substitution;
mod unification;
mod variable;

use model::*;
use parser::Program;

fn type_check(program: &Program) -> Result<Context, TypeError> {
    let mut context = Context::new();
    for (name, expr) in program {
        let (_, m, _) = algorithm::w(&context, expr, 0)?;
        context += (name.clone(), m.generalise(&context));
    }
    Ok(context)
}

fn main() {
    let source = r"
        id x = x
        const x y = x
        flip f x y = f y x
        main = flip const (id 0) (id true)
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(program) => match type_check(&program) {
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
