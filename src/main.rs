use chumsky::Parser;
use model::*;
use substitution::*;

mod lexer;
mod model;
mod parser;
mod substitution;

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

    let mut test = PolyType::Quantifier(
        "x".to_owned(),
        Box::new(PolyType::Mono(MonoType::Con(TypeConstructor::Function(
            Box::new(MonoType::Var("x".to_owned())),
            Box::new(MonoType::Con(TypeConstructor::Function(
                Box::new(MonoType::Var("y".to_owned())),
                Box::new(MonoType::Var("z".to_owned())),
            ))),
        )))),
    );

    let mut sub = Substitution::new();
    sub.insert("x".to_owned(), MonoType::Var("e".to_owned()));
    sub.insert(
        "y".to_owned(),
        MonoType::Con(TypeConstructor::Function(
            Box::new(MonoType::Var("a".to_owned())),
            Box::new(MonoType::Var("b".to_owned())),
        )),
    );

    let mut sub2 = Substitution::new();
    sub2.insert("a".to_owned(), MonoType::Var("c".to_owned()));

    sub.combine(&sub2);

    println!("{test}");

    test.substitute_mut(&sub);

    println!("{test}");
}
