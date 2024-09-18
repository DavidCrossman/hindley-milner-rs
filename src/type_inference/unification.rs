use super::{Result, TypeError};
use crate::model::typing::MonoType;
use crate::model::{Substitute, Substitution};

pub fn unify(m1: MonoType, m2: MonoType) -> Result<Substitution<MonoType>> {
    match (m1, m2) {
        (m1, m2) if m1 == m2 => Ok(Substitution::new()),
        (MonoType::Var(v), m) => {
            if m.vars().any(|v2| &v == v2) {
                Err(TypeError::InfiniteType(v, m))
            } else {
                Ok([(v, m)].into_iter().collect())
            }
        }
        (MonoType::App(l1, r1), MonoType::App(l2, r2)) => {
            let s1 = unify(*l1, *l2)?;
            let s2 = unify(r1.substitute(&s1), r2.substitute(&s1))?;
            Ok(s1.combine(&s2))
        }
        (m1, m2 @ MonoType::Var(_)) => unify(m2, m1),
        (m1, m2) => Err(TypeError::UnificationConflict(m1, m2)),
    }
}
