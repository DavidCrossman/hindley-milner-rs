use super::{unification::unify, Result, TypeError};
use crate::model::expression::{Binding, Expression, Literal};
use crate::model::typing::{MonoType, PolyType, TypeConstructor};
use crate::model::{Environment, Substitute, Substitution};

pub fn w(
    env: &Environment<PolyType>,
    expr: &Expression,
    n: usize,
) -> Result<(Substitution, MonoType, usize)> {
    match expr {
        Expression::Lit(lit) => Ok((
            Substitution::new(),
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Named("Unit".to_owned()),
                Literal::Int(_) => TypeConstructor::Named("Int".to_owned()),
            }),
            n,
        )),
        Expression::Var(x) => match env.get(x) {
            Some(p) => {
                let (m, n) = p.clone().instantiate(n);
                Ok((Substitution::new(), m, n))
            }
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let (s1, m1, n) = w(env, e1, n)?;
            let (s2, m2, n) = w(&env.clone().substitute(&s1), e2, n)?;
            let beta = MonoType::Var(n.into());
            let s3 = unify(MonoType::function(m2, beta.clone()), m1.substitute(&s2))?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
        Expression::Abs(b, e) => {
            let beta = MonoType::Var(n.into());
            let env = match b {
                Binding::Var(x) => &(env.clone() + (x.clone(), beta.clone().into())),
                Binding::Discard => env,
            };
            let (s, m, n) = w(env, e, n + 1)?;
            Ok((s.clone(), MonoType::function(beta, m).substitute(&s), n))
        }
        Expression::Let(b, e1, e2) => {
            let (s1, m1, n) = w(env, e1, n)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), m1.generalise(&env));
            }
            let (s2, m2, n) = w(&env, e2, n)?;
            Ok((s1.combine(&s2), m2, n))
        }
        Expression::Fix(f, b, e) => {
            let beta = MonoType::Var(n.into());
            let env = env.clone() + (f.clone(), beta.clone().into());
            let (s1, m1, n) = w(&env, &Expression::Abs(b.clone(), e.clone()), n + 1)?;
            let s2 = unify(beta.substitute(&s1), m1.clone())?;
            Ok((s1.combine(&s2), m1.substitute(&s2), n))
        }
    }
}

pub fn m(
    env: &Environment<PolyType>,
    expr: &Expression,
    t: MonoType,
    n: usize,
) -> Result<(Substitution, usize)> {
    match expr {
        Expression::Lit(lit) => unify(
            t,
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Named("Unit".to_owned()),
                Literal::Int(_) => TypeConstructor::Named("Int".to_owned()),
            }),
        )
        .map(|s| (s, n)),
        Expression::Var(x) => match env.get(x) {
            Some(p) => {
                let (t2, n) = p.clone().instantiate(n);
                unify(t, t2).map(|s| (s, n))
            }
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, e1, MonoType::function(beta.clone(), t), n + 1)?;
            let (s2, n) = m(&env.clone().substitute(&s1), e2, beta.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Abs(b, e) => {
            let beta1 = MonoType::Var(n.into());
            let beta2 = MonoType::Var((n + 1).into());
            let s1 = unify(t, MonoType::function(beta1.clone(), beta2.clone()))?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), beta1.substitute(&s1).into());
            }
            let (s2, n) = m(&env, e, beta2.substitute(&s1), n + 2)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Let(b, e1, e2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, e1, beta.clone(), n + 1)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), beta.substitute(&s1).generalise(&env));
            }
            let (s2, n) = m(&env, e2, t.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Fix(f, x, e) => {
            let env = env.clone() + (f.clone(), t.clone().into());
            m(&env, &Expression::Abs(x.clone(), e.clone()), t, n)
        }
    }
}
