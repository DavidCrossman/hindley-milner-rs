use super::model::MonoType;
use super::substitution::*;
use super::{Result, TypeError};

pub fn unify(m1: MonoType, m2: MonoType) -> Result<Substitution> {
    match (m1, m2) {
        (m1, m2) if m1 == m2 => Ok(Substitution::new()),
        (MonoType::Var(t), m) => {
            if m.vars().any(|t2| &t == t2) {
                Err(TypeError::InfiniteType(t, m))
            } else {
                Ok([(t, m)].into_iter().collect())
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
