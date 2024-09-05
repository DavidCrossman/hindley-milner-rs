use super::model::{MonoType, TypeConstructor, TypeError};
use super::substitution::*;
use super::Result;

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
        (m1 @ MonoType::Con(_), m2 @ MonoType::Var(_)) => unify(m2, m1),
        (MonoType::Con(c1), MonoType::Con(c2)) => unify_con(c1, c2),
    }
}

fn unify_con(c1: TypeConstructor, c2: TypeConstructor) -> Result<Substitution> {
    match (c1, c2) {
        (TypeConstructor::Function(l1, r1), TypeConstructor::Function(l2, r2)) => {
            let s1 = unify(*l1, *l2)?;
            let s2 = unify(r1.substitute(&s1), r2.substitute(&s1))?;
            Ok(s1.combine(&s2))
        }
        (TypeConstructor::List(t1), TypeConstructor::List(t2)) => unify(*t1, *t2),
        (c1, c2) => Err(TypeError::ConstructorConflict(c1, c2)),
    }
}
