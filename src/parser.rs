use crate::lexer::Token;
use crate::model::term::{Binding, Literal, Pattern, Term};
use crate::model::typing::{MonoType, Variable};
use crate::model::{DataConstructor, FreeVariable};
use crate::program::{Item, TypeDefinition};
use chumsky::{prelude::*, Error};

pub fn parse(tokens: &[Token]) -> Result<Vec<Item>, Vec<Simple<Token>>> {
    items_parser().parse(tokens)
}

fn binding_parser() -> impl Parser<Token, Binding, Error = Simple<Token>> + Copy {
    select! {
        Token::Ident(x) => Binding::Var(x),
        Token::Discard => Binding::Discard,
    }
}

fn paren_parser<O, E: Error<Token>>(
    inner: impl Parser<Token, O, Error = E> + Clone,
) -> impl Parser<Token, O, Error = E> + Clone {
    inner.delimited_by(
        just(Token::LeftParen),
        just(Token::Separator).or_not().then(just(Token::RightParen)),
    )
}

fn items_parser() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    let term_def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then(term_parser())
        .map(|(name, t)| Item::TermDefinition(name, t));

    let term_def_parser = term_def_parser.or(select! {Token::Ident(f) => f}
        .then(binding_parser())
        .then(binding_parser().repeated())
        .then_ignore(just(Token::Assign))
        .then(term_parser())
        .map(|(((name, b), bs), t)| {
            let t = bs.into_iter().rev().fold(t, |t, b| Term::Abs(b, Box::new(t)));
            let t = if b != name.clone().into() && t.free_vars().contains(&name) {
                Term::Fix(name.clone(), b, Box::new(t))
            } else {
                Term::Abs(b, Box::new(t))
            };
            Item::TermDefinition(name, t)
        }));

    let builtin_def_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::Assign))
        .then_ignore(just(Token::BuiltIn))
        .map(Item::BuiltInDefinition);

    let type_def_parser = just(Token::TypeDef)
        .ignore_then(select! {Token::Ident(x) => x})
        .then(select! {Token::Ident(x) => x}.map(Variable::Named).repeated())
        .then_ignore(just(Token::Assign))
        .then_with(|(name, vars)| {
            let return_type = vars.iter().fold(MonoType::Con(name.clone()), |m, v| {
                MonoType::App(Box::new(m), Box::new(MonoType::Var(v.clone())))
            });

            let data_con_parser = {
                select! {Token::Ident(x) => x}
                    .then(type_atom_parser().repeated())
                    .map(move |(name, params)| DataConstructor::new(name, return_type.clone(), params))
            };

            data_con_parser
                .separated_by(just(Token::TypeSum))
                .at_least(1)
                .map(move |cons| Item::TypeDefinition(TypeDefinition::new(name.clone(), vars.clone(), cons)))
        });

    let type_dec_parser = select! {Token::Ident(x) => x}
        .then_ignore(just(Token::OfType))
        .then(type_parser())
        .map(|(name, m)| Item::TypeDeclaration(name, m));

    let item_parser = choice((
        type_def_parser,
        term_def_parser,
        builtin_def_parser,
        type_dec_parser,
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

    let arrow_parser = just(Token::Arrow)
        .delimited_by(just(Token::LeftParen), just(Token::RightParen))
        .to(MonoType::Arrow);

    let app_parser = (atom.clone().then(atom.clone().repeated()))
        .foldl(|m1, m2| MonoType::App(Box::new(m1), Box::new(m2)));

    atom.define(choice((
        ident_parser,
        arrow_parser,
        paren_parser(type_parser.clone()),
    )));

    type_parser.define(
        (app_parser.clone().then_ignore(just(Token::Arrow)).repeated())
            .then(app_parser)
            .foldr(MonoType::function),
    );

    (atom, type_parser)
}

fn term_parser() -> impl Parser<Token, Term, Error = Simple<Token>> + Clone {
    recursive(|term_parser| {
        let var_parser = select! {Token::Ident(s) => Term::Var(s)};

        let literal_parser = select! {
            Token::Unit => Term::Lit(Literal::Unit),
            Token::Int(n) => Term::Lit(Literal::Int(n)),
        };

        let abs_parser = just(Token::Lambda)
            .ignore_then(binding_parser().repeated().at_least(1))
            .then_ignore(just(Token::Arrow))
            .then(term_parser.clone())
            .map(|(bs, t)| bs.into_iter().rev().fold(t, |t, b| Term::Abs(b, Box::new(t))));

        let let_parser = just(Token::Let)
            .ignore_then(binding_parser())
            .then_ignore(just(Token::Assign))
            .then(term_parser.clone())
            .then_ignore(just(Token::In))
            .then(term_parser.clone())
            .map(|((b, t1), t2)| Term::Let(b, Box::new(t1), Box::new(t2)));

        let let_parser = let_parser.or(just(Token::Let)
            .ignore_then(select! {Token::Ident(f) => f})
            .then(binding_parser())
            .then(binding_parser().repeated())
            .then_ignore(just(Token::Assign))
            .then(term_parser.clone())
            .then_ignore(just(Token::In))
            .then(term_parser.clone())
            .map(|((((f, b), bs), t1), t2)| {
                let t1 = bs.into_iter().rev().fold(t1, |t, b| Term::Abs(b, Box::new(t)));
                let t1 = if b != f.clone().into() && t1.free_vars().contains(&f) {
                    Term::Fix(f.clone(), b, Box::new(t1))
                } else {
                    Term::Abs(b, Box::new(t1))
                };
                Term::Let(f.into(), Box::new(t1), Box::new(t2))
            }));

        let pattern_parser = select! {Token::Ident(x) => x}
            .then(binding_parser().repeated())
            .map(|(constructor, bindings)| Pattern::new(constructor, bindings));

        let match_parser = just(Token::Match)
            .ignore_then(term_parser.clone())
            .then_ignore(just(Token::With))
            .then(
                pattern_parser
                    .then_ignore(just(Token::FatArrow))
                    .then(term_parser.clone())
                    .separated_by(just(Token::Separator))
                    .at_least(1),
            )
            .map(|(term, arms)| Term::Match(Box::new(term), arms));

        let term1_parser = choice((
            literal_parser,
            var_parser,
            abs_parser,
            let_parser,
            match_parser,
            paren_parser(term_parser),
        ));

        (term1_parser.clone())
            .then(term1_parser.repeated())
            .foldl(|t1, t2| Term::App(Box::new(t1), Box::new(t2)))
    })
}
