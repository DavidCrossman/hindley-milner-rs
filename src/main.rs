pub mod environment;
pub mod expression;
pub mod free_variable;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod type_checking;

use environment::Environment;
use expression::Literal;
use interpreter::{BuiltInFn, EvalError, Value};
use parser::Item;
use std::rc::Rc;

fn main() {
    let source = r"
        add : Int -> Int -> Int
        add = builtin
        main = add 5 7
    ";
    match lexer::lex(source) {
        Ok(tokens) => match parser::parse(&tokens) {
            Ok(items) => {
                let mut program = Vec::new();
                let mut env = Environment::new();
                let mut built_ins = Environment::new();
                items.into_iter().for_each(|item| match item {
                    Item::Definition(name, expr) => program.push((name, expr)),
                    Item::Declaration(name, m) => env += (name, m.generalise(&env)),
                    Item::BuiltInDefinition(s) => {
                        let fun: BuiltInFn = match s.as_str() {
                            "add" => Rc::new(move |x| {
                                if let Value::Lit(Literal::Int(x)) = x {
                                    Ok(Value::BuiltIn(Rc::new(move |y| {
                                        if let Value::Lit(Literal::Int(y)) = y {
                                            Ok(Value::Lit(Literal::Int(x + y)))
                                        } else {
                                            Err(EvalError::NoMain) //todo
                                        }
                                    })))
                                } else {
                                    Err(EvalError::NoMain) //todo
                                }
                            }),
                            _ => panic!("unknown builtin"),
                        };
                        built_ins += (s, fun);
                    }
                });
                match type_checking::type_check(&program, env) {
                    Ok(_) => match interpreter::run(&program, &built_ins) {
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
