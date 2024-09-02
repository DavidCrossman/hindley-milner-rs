use crate::expression::{Binding, Expression, Literal, Program};
use crate::lexer::Token;
use chumsky::prelude::*;

pub fn parse(tokens: &[Token]) -> Result<Program, Vec<Simple<Token>>> {
    program_parser().parse(tokens)
}

fn binding_parser() -> impl Parser<Token, Binding, Error = Simple<Token>> + Copy {
    select! {
        Token::Ident(x) => Binding::Var(x),
        Token::Discard => Binding::Discard,
    }
}

fn program_parser() -> impl Parser<Token, Program, Error = Simple<Token>> + Clone {
    let def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then(expr_parser());

    let def_parser = def_parser.or(select! {Token::Ident(f) => f}
        .then(binding_parser())
        .then(binding_parser().repeated())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|(((f, b), bs), e)| {
            let e = bs
                .into_iter()
                .rev()
                .fold(e, |e, b| Expression::Abs(b, Box::new(e)));
            let e = if b != f.clone().into() && e.free_vars().contains(&f) {
                Expression::Fix(f.clone(), b, Box::new(e))
            } else {
                Expression::Abs(b, Box::new(e))
            };
            (f, e)
        }));

    def_parser
        .separated_by(just(Token::Separator))
        .padded_by(just(Token::Separator).repeated())
        .then_ignore(end())
}

fn expr_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Unit => Expression::Lit(Literal::Unit),
            Token::Bool(b) => Expression::Lit(Literal::Bool(b)),
            Token::Int(n) => Expression::Lit(Literal::Nat(n)),
        };

        let abs_parser = just(Token::Lambda)
            .ignore_then(binding_parser().repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(xs, e)| {
                xs.into_iter()
                    .rev()
                    .fold(e, |e, b| Expression::Abs(b, Box::new(e)))
            });

        let let_parser = just(Token::Let)
            .ignore_then(binding_parser())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((b, e1), e2)| Expression::Let(b, Box::new(e1), Box::new(e2)));

        let let_parser = let_parser.or(just(Token::Let)
            .ignore_then(select! {Token::Ident(f) => f})
            .then(binding_parser())
            .then(binding_parser().repeated())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((((f, b), bs), e1), e2)| {
                let e1 = bs
                    .into_iter()
                    .rev()
                    .fold(e1, |e, b| Expression::Abs(b, Box::new(e)));
                let e1 = if b != f.clone().into() && e1.free_vars().contains(&f) {
                    Expression::Fix(f.clone(), b, Box::new(e1))
                } else {
                    Expression::Abs(b, Box::new(e1))
                };
                Expression::Let(f.into(), Box::new(e1), Box::new(e2))
            }));

        let paren_parser = expr.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let expr1_parser = choice((literal_parser, var_parser, abs_parser, let_parser, paren_parser));

        expr1_parser
            .clone()
            .then(expr1_parser.repeated())
            .foldl(|e1, e2| Expression::App(Box::new(e1), Box::new(e2)))
    })
}
