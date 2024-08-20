use crate::lexer::*;
use chumsky::prelude::*;
use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Nat(u64),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Lit(Literal),
    Var(String),
    App(Box<Expression>, Box<Expression>),
    Abs(String, Box<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Bool(b) => b.fmt(f),
            Literal::Nat(n) => n.fmt(f),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Lit(lit) => lit.fmt(f),
            Expression::Var(x) => x.fmt(f),
            Expression::App(e1, e2) => format!("({e1} {e2})").fmt(f),
            Expression::Abs(x, e) => format!("(λ{x} → {e})").fmt(f),
            Expression::Let(x, e1, e2) => format!("let {x} = {e1} in {e2}").fmt(f),
        }
    }
}

pub fn parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    recursive(|expr| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Bool(b) => Expression::Lit(Literal::Bool(b)),
            Token::Int(n) => Expression::Lit(Literal::Nat(n)),
        };

        let abs_parser = just(Token::Lambda)
            .ignore_then(select! {Token::Ident(x) => x}.repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(xs, e)| {
                xs.iter()
                    .rev()
                    .fold(e, |e, x| Expression::Abs(x.to_owned(), Box::new(e)))
            });

        let let_parser = just(Token::Let)
            .ignore_then(select! {Token::Ident(x) => x})
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((x, e1), e2)| Expression::Let(x, Box::new(e1), Box::new(e2)));

        let paren_parser = expr.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let expr1_parser = choice((
            literal_parser,
            var_parser,
            abs_parser,
            let_parser,
            paren_parser,
        ));

        expr1_parser
            .clone()
            .then(expr1_parser.repeated())
            .foldl(|e1, e2| Expression::App(Box::new(e1), Box::new(e2)))
    })
}
