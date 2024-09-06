pub mod algorithm;
pub mod model;
pub mod substitution;
pub mod unification;

pub type Result<T> = std::result::Result<T, model::TypeError>;
