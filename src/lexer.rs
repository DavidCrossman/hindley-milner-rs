use chumsky::prelude::*;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Ident(String),
    Lambda,
    Arrow,
    Let,
    Assign,
    In,
    LeftParen,
    RightParen,
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    choice((
        text::keyword("let").to(Token::Let),
        text::keyword("in").to(Token::In),
        one_of("λ\\").to(Token::Lambda),
        just("->").or(just("→")).to(Token::Arrow),
        just('=').to(Token::Assign),
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        text::ident().map(Token::Ident),
    ))
    .padded()
    .repeated()
    .then_ignore(end())
}
