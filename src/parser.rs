use crate::lexer::*;
use chumsky::prelude::*;

#[derive(Clone, Debug)]
pub enum Literal {
    Nat(u64),
}

#[derive(Clone, Debug)]
pub enum Expression {
    Lit(Literal),
    Var(String),
    App(Box<Expression>, Box<Expression>),
    Abs(String, Box<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
}

pub fn parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    recursive(|expr| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let int_parser = select! {Token::Int(n) => Expression::Lit(Literal::Nat(n))};

        let abs_parser = just(Token::Lambda)
            .ignore_then(select! {Token::Ident(s) => s}.repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(ss, e)| {
                ss.iter()
                    .rev()
                    .fold(e, |e, s| Expression::Abs(s.clone(), Box::new(e)))
            });

        let let_parser = just(Token::Let)
            .ignore_then(select! {Token::Ident(s) => s})
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((s, e1), e2)| Expression::Let(s, Box::new(e1), Box::new(e2)));

        let paren_parser = expr.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let parser_expr1 = choice((int_parser, var_parser, abs_parser, let_parser, paren_parser));

        parser_expr1
            .clone()
            .then(parser_expr1.repeated())
            .foldl(|a, b| Expression::App(Box::new(a), Box::new(b)))
    })
}