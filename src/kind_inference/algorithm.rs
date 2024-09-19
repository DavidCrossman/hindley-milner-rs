use super::{unify, KindError, Result, Substitution};
use crate::model::typing::{Kind, MonoType};
use crate::model::{Environment, Substitute};

pub fn w(env: &Environment<Kind>, mono: &MonoType, n: usize) -> Result<(Substitution, Kind, usize)> {
    match mono {
        MonoType::Var(v) => Err(KindError::FreeVariable(v.clone())),
        MonoType::Con(name) => match env.get(name) {
            Some(k) => Ok((Substitution::new(), k.clone(), n)),
            None => Err(KindError::UnknownTypeConstructor(name.clone())),
        },
        MonoType::Arrow => Ok((
            Substitution::new(),
            Kind::Arrow(
                Box::new(Kind::Type),
                Box::new(Kind::Arrow(Box::new(Kind::Type), Box::new(Kind::Type))),
            ),
            n,
        )),
        MonoType::App(m1, m2) => {
            let (s1, k1, n) = w(env, m1, n)?;
            let (s2, k2, n) = w(&env.clone().substitute(&s1), m2, n)?;
            let beta = Kind::Var(n.into());
            let k2 = Kind::Arrow(Box::new(k2), Box::new(beta.clone()));
            let s3 = unify(k2, k1.substitute(&s2))?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
    }
}

