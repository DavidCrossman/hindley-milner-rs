use super::{unification::unify, Result, TypeError};
use crate::model::term::{Binding, Literal, Term};
use crate::model::typing::{MonoType, PolyType, TypeConstructor};
use crate::model::{Environment, Substitute, Substitution};

pub fn w(env: &Environment<PolyType>, term: &Term, n: usize) -> Result<(Substitution, MonoType, usize)> {
    match term {
        Term::Lit(lit) => Ok((
            Substitution::new(),
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Named("Unit".to_owned()),
                Literal::Int(_) => TypeConstructor::Named("Int".to_owned()),
            }),
            n,
        )),
        Term::Var(v) => match env.get(v) {
            Some(p) => {
                let (m, n) = p.clone().instantiate(n);
                Ok((Substitution::new(), m, n))
            }
            None => Err(TypeError::UnknownVariable(v.clone())),
        },
        Term::App(t1, t2) => {
            let (s1, m1, n) = w(env, t1, n)?;
            let (s2, m2, n) = w(&env.clone().substitute(&s1), t2, n)?;
            let beta = MonoType::Var(n.into());
            let s3 = unify(MonoType::function(m2, beta.clone()), m1.substitute(&s2))?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
        Term::Abs(b, t) => {
            let beta = MonoType::Var(n.into());
            let env = match b {
                Binding::Var(x) => &(env.clone() + (x.clone(), beta.clone().into())),
                Binding::Discard => env,
            };
            let (s, m, n) = w(env, t, n + 1)?;
            Ok((s.clone(), MonoType::function(beta, m).substitute(&s), n))
        }
        Term::Let(b, t1, t2) => {
            let (s1, m1, n) = w(env, t1, n)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), m1.generalise(&env));
            }
            let (s2, m2, n) = w(&env, t2, n)?;
            Ok((s1.combine(&s2), m2, n))
        }
        Term::Fix(f, b, t) => {
            let beta = MonoType::Var(n.into());
            let env = env.clone() + (f.clone(), beta.clone().into());
            let (s1, m1, n) = w(&env, &Term::Abs(b.clone(), t.clone()), n + 1)?;
            let s2 = unify(beta.substitute(&s1), m1.clone())?;
            Ok((s1.combine(&s2), m1.substitute(&s2), n))
        }
    }
}

pub fn m(
    env: &Environment<PolyType>,
    term: &Term,
    mono: MonoType,
    n: usize,
) -> Result<(Substitution, usize)> {
    match term {
        Term::Lit(lit) => unify(
            mono,
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Named("Unit".to_owned()),
                Literal::Int(_) => TypeConstructor::Named("Int".to_owned()),
            }),
        )
        .map(|s| (s, n)),
        Term::Var(v) => match env.get(v) {
            Some(p) => {
                let (m2, n) = p.clone().instantiate(n);
                unify(mono, m2).map(|s| (s, n))
            }
            None => Err(TypeError::UnknownVariable(v.clone())),
        },
        Term::App(t1, t2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, t1, MonoType::function(beta.clone(), mono), n + 1)?;
            let (s2, n) = m(&env.clone().substitute(&s1), t2, beta.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Term::Abs(b, t) => {
            let beta1 = MonoType::Var(n.into());
            let beta2 = MonoType::Var((n + 1).into());
            let s1 = unify(mono, MonoType::function(beta1.clone(), beta2.clone()))?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), beta1.substitute(&s1).into());
            }
            let (s2, n) = m(&env, t, beta2.substitute(&s1), n + 2)?;
            Ok((s1.combine(&s2), n))
        }
        Term::Let(b, t1, t2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, t1, beta.clone(), n + 1)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), beta.substitute(&s1).generalise(&env));
            }
            let (s2, n) = m(&env, t2, mono.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Term::Fix(f, x, t) => {
            let env = env.clone() + (f.clone(), mono.clone().into());
            m(&env, &Term::Abs(x.clone(), t.clone()), mono, n)
        }
    }
}
