use chumsky::prelude::*;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Ident(String),
    Unit,
    Bool(bool),
    Int(u64),
    Lambda,
    Arrow,
    Let,
    Rec,
    Assign,
    In,
    LeftParen,
    RightParen,
}

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    choice((
        text::keyword("let").to(Token::Let),
        text::keyword("rec").to(Token::Rec),
        text::keyword("in").to(Token::In),
        text::keyword("true").to(Token::Bool(true)),
        text::keyword("false").to(Token::Bool(false)),
        just("()").to(Token::Unit),
        one_of("λ\\").to(Token::Lambda),
        just("->").or(just("→")).to(Token::Arrow),
        just('=').to(Token::Assign),
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        text::digits(10).try_map(|x: String, span| {
            x.parse()
                .map(Token::Int)
                .map_err(|e| Simple::custom(span, format!("{}", e)))
        }),
        text::ident().map(Token::Ident),
    ))
    .padded()
    .repeated()
    .then_ignore(end())
}
