use chumsky::Parser;
use model::*;
use substitution::Substitute;

mod algorithm;
mod interpreter;
mod lexer;
mod model;
mod parser;
mod substitution;
mod unification;
mod variable;

fn main() {
    match lexer::lexer().parse("let id = λx -> x in (λa b -> a) (id ()) (id 0)") {
        Ok(tokens) => match parser::parser().parse(tokens) {
            Ok(expr) => {
                println!("type checking expression: {expr}");
                println!("algorithm w");
                match algorithm::w(&Context::new(), &expr, 0) {
                    Ok((_, m, _)) => println!("type: {m}"),
                    Err(e) => println!("error: {e}"),
                }
                println!("algorithm m");
                let t = MonoType::Var(TypeVariable::UserDefined("a".to_owned()));
                match algorithm::m(&Context::new(), &expr, t.clone(), 0) {
                    Ok((s, _)) => println!("type: {}", t.substitute(&s)),
                    Err(e) => println!("error: {e}"),
                }
                println!("evaluation");
                match interpreter::eval(&expr) {
                    Ok(e) => println!("success: {e}"),
                    Err(e) => println!("error: {e}"),
                }
            }
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
