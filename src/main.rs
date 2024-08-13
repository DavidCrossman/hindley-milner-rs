use chumsky::{prelude::*, Parser};

#[derive(Clone, Debug)]
enum Token {
    Ident(String),
    Lambda,
    Arrow,
    Let,
    Assign,
    In,
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    choice((
        text::keyword("let").to(Token::Let),
        text::keyword("in").to(Token::In),
        one_of("λ\\").to(Token::Lambda),
        just("->").or(just("→")).to(Token::Arrow),
        just('=').to(Token::Assign),
        text::ident().map(Token::Ident),
    ))
    .padded()
    .repeated()
    .then_ignore(end())
}

fn main() {
    let x = lexer().parse("let x = y z in \\ph -> ph x");
    println!("{x:?}");
}
