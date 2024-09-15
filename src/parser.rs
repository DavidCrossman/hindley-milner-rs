use crate::expression::{Binding, Expression, Literal};
use crate::type_checking::model::{MonoType, TypeConstructor};
use crate::{free_variable::FreeVariable, lexer::Token};
use chumsky::prelude::*;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct DataConstructor {
    pub name: String,
    pub params: Vec<TypeConstructor>,
}

#[derive(Clone, Debug)]
pub enum Item {
    ValueDefinition(String, Expression),
    BuiltInDefinition(String),
    TypeDefinition(String, Vec<DataConstructor>),
    Declaration(String, MonoType),
}

impl Display for DataConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let product = self.params.iter().map(ToString::to_string).collect::<Vec<_>>();
        write!(f, "{} {}", self.name, product.join("×"))
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ValueDefinition(s, e) => write!(f, "{s} = {e}"),
            Self::BuiltInDefinition(s) => write!(f, "{s} = builtin"),
            Self::TypeDefinition(s, cons) if !cons.is_empty() => {
                let sum = cons.iter().map(ToString::to_string).collect::<Vec<_>>();
                write!(f, "{s} = {}", sum.join(" + "))
            }
            Self::TypeDefinition(s, _) => write!(f, "type {s}"),
            Self::Declaration(s, p) => write!(f, "{s} : {p}"),
        }
    }
}

pub fn parse(tokens: &[Token]) -> Result<Vec<Item>, Vec<Simple<Token>>> {
    items_parser().parse(tokens)
}

fn binding_parser() -> impl Parser<Token, Binding, Error = Simple<Token>> + Copy {
    select! {
        Token::ValueIdent(x) => Binding::Var(x),
        Token::Discard => Binding::Discard,
    }
}

fn type_con_parser() -> impl Parser<Token, TypeConstructor, Error = Simple<Token>> + Copy {
    select! {
        Token::UnitType => TypeConstructor::Unit,
        Token::IntType => TypeConstructor::Int,
        Token::TypeIdent(x) => TypeConstructor::Custom(x),
    }
}

fn items_parser() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    let def_parser = select! {Token::ValueIdent(x) => x}
        .then_ignore(just(Token::Assign))
        .then(expr_parser());

    let def_parser = def_parser.or(select! {Token::ValueIdent(f) => f}
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

    let builtin_def_parser = select! {Token::ValueIdent(x) => x}
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::BuiltIn));

    let data_con_parser = select! {Token::ValueIdent(x) => x}
        .then(type_con_parser().repeated())
        .map(|(name, params)| DataConstructor { name, params });

    let type_def_parser = select! {Token::TypeIdent(x) => x}
        .then_ignore(just(Token::Assign))
        .then(data_con_parser.separated_by(just(Token::TypeSum)));

    let dec_parser = select! {Token::ValueIdent(x) => x}
        .then_ignore(just(Token::OfType))
        .then(type_parser());

    let item_parser = choice((
        def_parser.map(|(name, e)| Item::ValueDefinition(name, e)),
        builtin_def_parser.map(Item::BuiltInDefinition),
        type_def_parser.map(|(name, cons)| Item::TypeDefinition(name, cons)),
        dec_parser.map(|(name, m)| Item::Declaration(name, m)),
    ));

    item_parser
        .separated_by(just(Token::Separator))
        .padded_by(just(Token::Separator).repeated())
        .then_ignore(end())
}

fn type_parser() -> impl Parser<Token, MonoType, Error = Simple<Token>> + Clone {
    recursive(|type_parser| {
        let ident_parser = select! {Token::ValueIdent(x) => MonoType::Var(x.into()) };

        let paren_parser = type_parser
            .clone()
            .delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let atom = choice((type_con_parser().map(MonoType::Con), ident_parser, paren_parser));

        (atom.clone().then_ignore(just(Token::Arrow)).repeated())
            .then(atom)
            .foldr(|a, b| MonoType::Con(TypeConstructor::Function(Box::new(a), Box::new(b))))
    })
}

fn expr_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        let var_parser = select! {Token::ValueIdent(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Unit => Expression::Lit(Literal::Unit),
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
            .ignore_then(select! {Token::ValueIdent(f) => f})
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
