pub mod algorithm;
pub mod model;
pub mod substitution;
pub mod unification;

pub type Result<T> = std::result::Result<T, model::TypeError>;

use crate::{environment::Environment, expression::Program};
use model::{MonoType, PolyType};
use substitution::Substitute;

pub fn type_check(program: &Program, mut env: Environment<PolyType>) -> Result<Environment<PolyType>> {
    for (name, expr) in program {
        let (t, n) = match env.remove(name) {
            Some(p) => p.instantiate(0),
            None => (MonoType::Var(0.into()), 1),
        };
        let (s, _) = algorithm::m(&env, expr, t.clone(), n)?;
        env += (name.clone(), t.substitute(&s).generalise(&env));
    }
    Ok(env)
}
