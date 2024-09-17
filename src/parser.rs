use crate::expression::{Binding, Expression, Literal};
use crate::type_checking::model::{MonoType, TypeVariable};
use crate::{free_variable::FreeVariable, lexer::Token};
use chumsky::prelude::*;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct DataConstructor {
    pub name: String,
    pub types: Vec<MonoType>,
}

#[derive(Clone, Debug)]
pub enum Item {
    TypeDefinition(String, Vec<TypeVariable>, Vec<DataConstructor>),
    TermDefinition(String, Expression),
    BuiltInDefinition(String),
    Declaration(String, MonoType),
}

impl Display for DataConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.types.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let product = self
                .types
                .iter()
                .map(|m| match m {
                    MonoType::App(..) => format!("({m})"),
                    MonoType::Var(_) | MonoType::Con(_) => m.to_string(),
                })
                .collect::<Vec<_>>();
            write!(f, "{} {}", self.name, product.join(" "))
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeDefinition(s, params, cons) => {
                let mut s = format!("type {s}");
                if !params.is_empty() {
                    let params = params.iter().map(ToString::to_string).collect::<Vec<_>>();
                    s = format!("{s} {}", params.join(" "));
                }
                if !cons.is_empty() {
                    let sum = cons.iter().map(ToString::to_string).collect::<Vec<_>>();
                    s = format!("{s} = {}", sum.join(" + "));
                }
                write!(f, "{s}")
            }
            Self::TermDefinition(s, e) => write!(f, "{s} = {e}"),
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
    let term_def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then(expr_parser());

    let term_def_parser = term_def_parser.or(select! {Token::Ident(f) => f}
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

    let data_con_parser = select! {Token::Ident(x) => x}
        .then(type_atom_parser().repeated())
        .map(|(name, types)| DataConstructor { name, types });

    let type_def_parser = just(Token::TypeDef)
        .ignore_then(select! {Token::Ident(x) => x})
        .then(select! {Token::Ident(x) => x}.map(TypeVariable::Named).repeated())
        .then_ignore(just(Token::Assign))
        .then(data_con_parser.separated_by(just(Token::TypeSum)).at_least(1));

    let dec_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::OfType))
        .then(type_parser());

    let item_parser = choice((
        type_def_parser.map(|((name, vars), cons)| Item::TypeDefinition(name, vars, cons)),
        term_def_parser.map(|(name, e)| Item::TermDefinition(name, e)),
        builtin_def_parser.map(Item::BuiltInDefinition),
        dec_parser.map(|(name, m)| Item::Declaration(name, m)),
    ));

    item_parser
        .separated_by(just(Token::Separator))
        .padded_by(just(Token::Separator).repeated())
        .then_ignore(end())
}

fn type_parser() -> impl Parser<Token, MonoType, Error = Simple<Token>> + Clone {
    create_type_parsers().1
}

fn type_atom_parser() -> impl Parser<Token, MonoType, Error = Simple<Token>> + Clone {
    create_type_parsers().0
}

fn create_type_parsers() -> (
    impl Parser<Token, MonoType, Error = Simple<Token>> + Clone,
    impl Parser<Token, MonoType, Error = Simple<Token>> + Clone,
) {
    let mut atom = Recursive::declare();
    let mut type_parser = Recursive::declare();

    let ident_parser = select! {Token::Ident(x) => MonoType::Var(x.into()) };

    let paren_parser = (type_parser.clone()).delimited_by(just(Token::LeftParen), just(Token::RightParen));

    let app_parser = (atom.clone().then(atom.clone().repeated()))
        .foldl(|m1, m2| MonoType::App(Box::new(m1), Box::new(m2)));

    atom.define(ident_parser.or(paren_parser));

    type_parser.define(
        (app_parser.clone().then_ignore(just(Token::Arrow)).repeated())
            .then(app_parser)
            .foldr(MonoType::function),
    );

    (atom, type_parser)
}

fn expr_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> + Clone {
    recursive(|expr_parser| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Unit => Expression::Lit(Literal::Unit),
            Token::Int(n) => Expression::Lit(Literal::Int(n)),
        };

        let abs_parser = just(Token::Lambda)
            .ignore_then(binding_parser().repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(expr_parser.clone())
            .map(|(xs, e)| {
                xs.into_iter()
                    .rev()
                    .fold(e, |e, b| Expression::Abs(b, Box::new(e)))
            });

        let let_parser = just(Token::Let)
            .ignore_then(binding_parser())
            .then_ignore(just(Token::Assign))
            .then(expr_parser.clone())
            .then_ignore(just(Token::In))
            .then(expr_parser.clone())
            .map(|((b, e1), e2)| Expression::Let(b, Box::new(e1), Box::new(e2)));

        let let_parser = let_parser.or(just(Token::Let)
            .ignore_then(select! {Token::Ident(f) => f})
            .then(binding_parser())
            .then(binding_parser().repeated())
            .then_ignore(just(Token::Assign))
            .then(expr_parser.clone())
            .then_ignore(just(Token::In))
            .then(expr_parser.clone())
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

        let paren_parser = expr_parser.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let expr1_parser = choice((literal_parser, var_parser, abs_parser, let_parser, paren_parser));

        expr1_parser
            .clone()
            .then(expr1_parser.repeated())
            .foldl(|e1, e2| Expression::App(Box::new(e1), Box::new(e2)))
    })
}
