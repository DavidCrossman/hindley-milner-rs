use chumsky::Parser;
use model::*;

mod algorithm;
mod lexer;
mod model;
mod parser;
mod substitution;
mod unification;
mod variable;

fn main() {
    match lexer::lexer().parse("let id = λx -> x in (λa b -> a) (id true) (id 0)") {
        Ok(tokens) => match parser::parser().parse(tokens) {
            Ok(expr) => match algorithm::w(&Context::new(), &expr, 0) {
                Ok((s, m, _)) => println!("type checking {expr} succeeded\ntype: {m}\nsubst:{s}"),
                Err(e) => println!("type checking {expr} failed\nerror: {e}"),
            },
            Err(e) => println!("parse error: {e:?}"),
        },
        Err(e) => println!("lexical error: {e:?}"),
    }
}
