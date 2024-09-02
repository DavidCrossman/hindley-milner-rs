use crate::{lexer::*, variable::FreeVariable};
use chumsky::prelude::*;
use std::{collections::HashMap, fmt::Display};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
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
    Closure(String, Box<Expression>, Environment),
    Fix(String, String, Box<Expression>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Environment(pub HashMap<String, Expression>);

pub type Program = Vec<(String, Expression)>;

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => "()".fmt(f),
            Literal::Bool(b) => b.fmt(f),
            Literal::Nat(n) => n.fmt(f),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Lit(lit) => lit.fmt(f),
            Var(x) => x.fmt(f),
            App(e1, e2) => match **e1 {
                Lit(_) | Var(_) => match **e2 {
                    Lit(_) | Var(_) => write!(f, "{e1} {e2}"),
                    App(..) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
                Abs(..) | Let(..) | Closure(..) | Fix(..) => match **e2 {
                    Lit(_) | Var(_) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "({e1}) {e2}"),
                    App(..) => write!(f, "({e1}) ({e2})"),
                },
                App(..) => match **e2 {
                    Lit(_) | Var(_) => write!(f, "{e1} {e2}"),
                    App(..) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
            },
            Abs(x, e) => write!(f, "λ{x} → {e}"),
            Let(x, e1, e2) => write!(f, "let {x} = {e1} in {e2}"),
            Closure(x, e, env) => write!(f, "λ{env} {x} → {e}"),
            Fix(fun, x, e) => write!(f, "fix {fun} λ{x} → {e}"),
        }
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.0
                .iter()
                .map(|(k, v)| format!("{k}={v}"))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl FromIterator<(String, Expression)> for Environment {
    fn from_iter<T: IntoIterator<Item = (String, Expression)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}

impl Expression {
    pub fn is_value(&self) -> bool {
        use Expression::*;
        match self {
            Lit(_) | Closure(..) => true,
            Var(_) | App(..) | Abs(..) | Let(..) | Fix(..) => false,
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

pub fn parse(tokens: &[Token]) -> Result<Program, Vec<Simple<Token>>> {
    program_parser().parse(tokens)
}

fn program_parser() -> impl Parser<Token, Program, Error = Simple<Token>> {
    let def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then(expr_parser());

    let def_parser = def_parser.or(select! {Token::Ident(f) => f}
        .then(select! {Token::Ident(x) => x})
        .then(select! {Token::Ident(x) => x}.repeated())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|(((f, x), xs), e)| {
            let e = xs
                .into_iter()
                .rev()
                .fold(e, |e, x| Expression::Abs(x, Box::new(e)));
            let e = if f != x && e.free_vars().contains(&f) {
                Expression::Fix(f.clone(), x, Box::new(e))
            } else {
                Expression::Abs(x, Box::new(e))
            };
            (f, e)
        }));

    def_parser
        .separated_by(just(Token::Separator))
        .padded_by(just(Token::Separator).repeated())
        .then_ignore(end())
}

fn expr_parser() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    recursive(|expr| {
        let var_parser = select! {Token::Ident(s) => Expression::Var(s)};

        let literal_parser = select! {
            Token::Unit => Expression::Lit(Literal::Unit),
            Token::Bool(b) => Expression::Lit(Literal::Bool(b)),
            Token::Int(n) => Expression::Lit(Literal::Nat(n)),
        };

        let abs_parser = just(Token::Lambda)
            .ignore_then(select! {Token::Ident(x) => x}.repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(xs, e)| {
                xs.into_iter()
                    .rev()
                    .fold(e, |e, x| Expression::Abs(x, Box::new(e)))
            });

        let let_parser = just(Token::Let)
            .ignore_then(select! {Token::Ident(x) => x})
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((x, e1), e2)| Expression::Let(x, Box::new(e1), Box::new(e2)));

        let let_parser = let_parser.or(just(Token::Let)
            .ignore_then(select! {Token::Ident(f) => f})
            .then(select! {Token::Ident(x) => x})
            .then(select! {Token::Ident(x) => x}.repeated())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((((f, x), xs), e1), e2)| {
                let e1 = xs
                    .into_iter()
                    .rev()
                    .fold(e1, |e, x| Expression::Abs(x, Box::new(e)));
                let e1 = if f != x && e1.free_vars().contains(&f) {
                    Expression::Fix(f.clone(), x, Box::new(e1))
                } else {
                    Expression::Abs(x, Box::new(e1))
                };
                Expression::Let(f, Box::new(e1), Box::new(e2))
            }));

        let paren_parser = expr.delimited_by(just(Token::LeftParen), just(Token::RightParen));

        let expr1_parser = choice((literal_parser, var_parser, abs_parser, let_parser, paren_parser));

        expr1_parser
            .clone()
            .then(expr1_parser.repeated())
            .foldl(|e1, e2| Expression::App(Box::new(e1), Box::new(e2)))
    })
}
