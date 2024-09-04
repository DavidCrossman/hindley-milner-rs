use chumsky::{prelude::*, text::newline};

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Token {
    Let,
    In,
    Lambda,
    Arrow,
    Assign,
    OfType,
    LeftParen,
    RightParen,
    UnitType,
    BoolType,
    IntType,
    Unit,
    Bool(bool),
    Int(u64),
    Ident(String),
    Discard,
    Separator,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
enum PaddedToken {
    Tok(Token),
    Newline,
}

pub fn lex(source: &str) -> Result<Vec<Token>, Vec<Simple<char>>> {
    let mut tokens = Vec::new();
    lexer().parse(source)?.into_iter().for_each(|t| match t {
        PaddedToken::Tok(t) => tokens.push(t),
        PaddedToken::Newline => {
            use Token::*;
            if tokens.last().is_some_and(|t| {
                matches!(
                    t,
                    UnitType | BoolType | IntType | Ident(_) | Unit | Int(_) | Bool(_) | RightParen
                )
            }) {
                tokens.push(Token::Separator);
            }
        }
    });
    Ok(tokens)
}

fn lexer() -> impl Parser<char, Vec<PaddedToken>, Error = Simple<char>> + Clone {
    let ident_lexer = filter(|c: &char| c.is_ascii_alphabetic())
        .chain::<char, _, _>(filter(|c: &char| c.is_ascii_alphanumeric() || c == &'_').repeated())
        .collect();

    let token_lexer = choice((
        text::keyword("let").to(Token::Let),
        text::keyword("in").to(Token::In),
        text::keyword("Unit").to(Token::UnitType),
        text::keyword("Bool").to(Token::BoolType),
        text::keyword("Int").to(Token::IntType),
        text::keyword("true").to(Token::Bool(true)),
        text::keyword("false").to(Token::Bool(false)),
        just("()").to(Token::Unit),
        one_of("λ\\").to(Token::Lambda),
        just("->").or(just("→")).to(Token::Arrow),
        just('=').to(Token::Assign),
        just(':').to(Token::OfType),
        just('(').to(Token::LeftParen),
        just(')').to(Token::RightParen),
        just(';').to(Token::Separator),
        text::digits(10).try_map(|x: String, span| {
            x.parse()
                .map(Token::Int)
                .map_err(|e| Simple::custom(span, format!("{}", e)))
        }),
        ident_lexer.map(Token::Ident),
        just('_')
            .then(filter(|c: &char| c.is_ascii_alphanumeric() || c == &'_').repeated())
            .to(Token::Discard),
    ));

    let tokens_lexer = token_lexer
        .map(PaddedToken::Tok)
        .or(newline().to(PaddedToken::Newline))
        .padded_by(filter(|c: &char| c != &'\n' && c.is_whitespace()).repeated())
        .repeated();

    tokens_lexer.then_ignore(end())
}
