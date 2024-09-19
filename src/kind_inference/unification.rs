use super::{KindError, Result, Substitution};
use crate::model::typing::Kind;
use crate::model::Substitute;

pub fn unify(k1: Kind, k2: Kind) -> Result<Substitution> {
    match (k1, k2) {
        (k1, k2) if k1 == k2 => Ok(Substitution::new()),
        (Kind::Var(v), k) => {
            if k.vars().any(|v2| &v == v2) {
                Err(KindError::InfiniteKind(v, k))
            } else {
                Ok([(v, k)].into_iter().collect())
            }
        }
        (Kind::Arrow(l1, r1), Kind::Arrow(l2, r2)) => {
            let s1 = unify(*l1, *l2)?;
            let s2 = unify(r1.substitute(&s1), r2.substitute(&s1))?;
            Ok(s1.combine(&s2))
        }
        (k1, k2 @ Kind::Var(_)) => unify(k2, k1),
        (k1, k2) => Err(KindError::UnificationConflict(k1, k2)),
    }
}
