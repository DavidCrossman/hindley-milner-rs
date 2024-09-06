use crate::expression::{Binding, Expression, Literal};
use crate::type_checking::model::{MonoType, TypeConstructor};
use crate::{free_variable::FreeVariable, lexer::Token};
use chumsky::prelude::*;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Item {
    Definition(String, Expression),
    BuiltInDefinition(String),
    Declaration(String, MonoType),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Definition(s, e) => write!(f, "{s} = {e}"),
            Self::BuiltInDefinition(s) => write!(f, "{s} = builtin"),
            Self::Declaration(s, p) => write!(f, "{s} : {p}"),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Item>, Vec<Simple<Token>>> {
    items_parser().parse(tokens)
}

fn binding_parser() -> impl Parser<Token, Binding, Error = Simple<Token>> + Copy {
    select! {
        Token::Ident(x) => Binding::Var(x),
        Token::Discard => Binding::Discard,
    }
}

fn items_parser() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
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

    let builtin_def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::BuiltIn));

    let dec_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::OfType))
        .then(type_parser());

    let item_parser = def_parser
        .map(|(name, e)| Item::Definition(name, e))
        .or(builtin_def_parser.map(Item::BuiltInDefinition))
        .or(dec_parser.map(|(name, m)| Item::Declaration(name, m)));

    item_parser
        .separated_by(just(Token::Separator))
        .padded_by(just(Token::Separator).repeated())
        .then_ignore(end())
}

fn type_parser() -> impl Parser<Token, MonoType, Error = Simple<Token>> + Clone {
    recursive(|type_parser| {
        let type_con_parser = select! {
            Token::UnitType => MonoType::Con(TypeConstructor::Unit),
            Token::BoolType => MonoType::Con(TypeConstructor::Bool),
            Token::IntType => MonoType::Con(TypeConstructor::Int),
        };

        let ident_parser = select! {Token::Ident(x) => MonoType::Var(x.into()) };

        let paren_parser = type_parser
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let atom = choice((type_con_parser, ident_parser, paren_parser));

        (atom.clone().then_ignore(just(Token::Arrow)).repeated())
            .then(atom)
            .foldr(|a, b| MonoType::Con(TypeConstructor::Function(Box::new(a), Box::new(b))))
    })
}

fn expr_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Unit => Expression::Lit(Literal::Unit),
            Token::Bool(b) => Expression::Lit(Literal::Bool(b)),
            Token::Int(n) => Expression::Lit(Literal::Int(n)),
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
