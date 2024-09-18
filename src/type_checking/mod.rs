pub mod algorithm;
pub mod model;
pub mod substitution;
pub mod unification;

use model::{MonoType, Variable};
use thiserror::Error;

#[derive(Clone, Error, Debug)]
pub enum TypeError {
    #[error("variable '{0}' is not defined")]
    UnknownVariable(String),
    #[error("cannot construct infinite type {0} = {1}")]
    InfiniteType(Variable, MonoType),
    #[error("expected type {0}, found {1}")]
    UnificationConflict(MonoType, MonoType),
}

pub type Result<T> = std::result::Result<T, TypeError>;
