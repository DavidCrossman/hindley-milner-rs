pub mod term;
pub mod typing;

mod environment;
mod free_variable;
mod substitution;

pub use environment::Environment;
pub use free_variable::FreeVariable;
pub use substitution::{Substitute, Substitution};
