pub mod algorithm;
pub mod model;
pub mod substitution;
pub mod unification;
pub mod variable;

use crate::expression::Program;
use model::{Context, MonoType, TypeError};
use substitution::Substitute;

pub fn type_check(program: &Program, mut context: Context) -> Result<Context, TypeError> {
    for (name, expr) in program {
        let (t, n) = match context.map.remove(name) {
            Some(p) => p.instantiate(0),
            None => (MonoType::Var(0.into()), 1),
        };
        let (s, _) = algorithm::m(&context, expr, t.clone(), n)?;
        context += (name.clone(), t.substitute(&s).generalise(&context));
    }
    Ok(context)
}
