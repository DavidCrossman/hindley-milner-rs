pub mod algorithm;
pub mod model;
pub mod substitution;
pub mod unification;
pub mod variable;

use crate::expression::Program;
use model::{Context, TypeError};

pub fn type_check(program: &Program) -> Result<Context, TypeError> {
    let mut context = Context::new();
    for (name, expr) in program {
        let (_, m, _) = algorithm::w(&context, expr, 0)?;
        context += (name.clone(), m.generalise(&context));
    }
    Ok(context)
}
