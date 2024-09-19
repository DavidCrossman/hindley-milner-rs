mod unification;

pub use unification::unify;

use crate::model::typing::{Kind, Variable};
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum KindError {
    #[error("cannot construct infinite kind {0} = {1}")]
    InfiniteKind(Variable, Kind),
    #[error("expected kind {0}, found {1}")]
    UnificationConflict(Kind, Kind),
}

pub type Result<T> = std::result::Result<T, KindError>;

type Substitution = crate::model::Substitution<Kind>;
