pub mod algorithm;

mod unification;

pub use unification::unify;

use crate::model::term::Term;
use crate::model::typing::{MonoType, PolyType, Variable};
use crate::model::{DataConstructor, Environment, Substitute};
use std::collections::HashSet;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum TypeError {
    #[error("variable '{0}' is not defined")]
    UnknownVariable(String),
    #[error("data constructor '{0}' is not defined")]
    UnknownDataConstructor(String),
    #[error("cannot construct infinite type {0} = {1}")]
    InfiniteType(Variable, MonoType),
    #[error("expected type {0}, found {1}")]
    UnificationConflict(MonoType, MonoType),
    #[error("match missing cases {}", .0.iter().map(|s| format!("'{s}'")).collect::<Vec<_>>().join(", "))]
    InexhausiveMatch(HashSet<String>),
    #[error("expected {1} parameters for constructor '{0}', found {2}")]
    IncorrectPatternParameters(String, usize, usize),
}

pub type Result<T> = std::result::Result<T, TypeError>;

type Substitution = crate::model::Substitution<MonoType>;

pub fn infer_type(
    name: &str,
    term: &Term,
    type_env: &Environment<PolyType>,
    data_cons: &Environment<DataConstructor>,
) -> Result<PolyType> {
    let (mono, n) = match type_env.get(name) {
        Some(p) => p.clone().instantiate(0),
        None => (MonoType::Var(0.into()), 1),
    };
    let (s, _) = algorithm::m(type_env, term, data_cons, mono.clone(), n)?;
    Ok(mono.substitute(&s).generalise(type_env))
}
