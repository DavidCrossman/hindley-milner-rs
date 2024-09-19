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
        MonoType::Arrow => {
            let k = Kind::arrow(Kind::Type, Kind::arrow(Kind::Type, Kind::Type));
            Ok((Substitution::new(), k, n))
        }
        MonoType::App(m1, m2) => {
            let (s1, k1, n) = w(env, m1, n)?;
            let (s2, k2, n) = w(&env.clone().substitute(&s1), m2, n)?;
            let beta = Kind::Var(n.into());
            let s3 = unify(Kind::arrow(k2, beta.clone()), k1.substitute(&s2))?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
    }
}

pub fn m(env: &Environment<Kind>, mono: &MonoType, k: Kind, n: usize) -> Result<(Substitution, usize)> {
    match mono {
        MonoType::Var(v) => Err(KindError::FreeVariable(v.clone())),
        MonoType::Con(name) => match env.get(name) {
            Some(k2) => unify(k, k2.clone()).map(|s| (s, n)),
            None => Err(KindError::UnknownTypeConstructor(name.clone())),
        },
        MonoType::Arrow => {
            let k2 = Kind::arrow(Kind::Type, Kind::arrow(Kind::Type, Kind::Type));
            unify(k, k2).map(|s| (s, n))
        }
        MonoType::App(m1, m2) => {
            let beta = Kind::Var(n.into());
            let (s1, n) = m(env, m1, Kind::arrow(beta.clone(), k), n + 1)?;
            let (s2, n) = m(&env.clone().substitute(&s1), m2, beta.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
    }
}
