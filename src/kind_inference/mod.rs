pub mod algorithm;

mod unification;

pub use unification::unify;

use crate::model::typing::{Kind, PolyType, Variable};
use crate::model::Environment;
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum KindError {
    #[error("cannot infer the kind of free type variable '{0}'")]
    FreeVariable(Variable),
    #[error("unknown type constructor '{0}'")]
    UnknownTypeConstructor(String),
    #[error("cannot construct infinite kind {0} = {1}")]
    InfiniteKind(Variable, Kind),
    #[error("expected kind {0}, found {1}")]
    UnificationConflict(Kind, Kind),
}

pub type Result<T> = std::result::Result<T, KindError>;

type Substitution = crate::model::Substitution<Kind>;

pub fn kind_check(p: &PolyType, k: Kind, type_constructors: &Environment<Kind>) -> Result<()> {
    let mut env = type_constructors.clone();
    env.extend(p.quantifiers.iter().map(|v| (v.to_name(), Kind::Var(v.clone()))));
    let mono = p.mono.clone().substitute_constructors(&env);
    algorithm::m(&env, &mono, k, 0)?;
    Ok(())
}
