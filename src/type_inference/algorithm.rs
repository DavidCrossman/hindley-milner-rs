use super::{unify, Result, Substitution, TypeError};
use crate::model::term::{Binding, Literal, Pattern, Term};
use crate::model::typing::{MonoType, PolyType};
use crate::model::{DataConstructor, Environment, Substitute};
use std::collections::HashSet;

pub fn w(
    env: &Environment<PolyType>,
    term: &Term,
    data_cons: &Environment<DataConstructor>,
    n: usize,
) -> Result<(Substitution, MonoType, usize)> {
    match term {
        Term::Lit(lit) => {
            let m = MonoType::Con(match lit {
                Literal::Unit => "Unit".to_owned(),
                Literal::Int(_) => "Int".to_owned(),
            });
            Ok((Substitution::new(), m, n))
        }
        Term::Var(v) => match env.get(v) {
            Some(p) => {
                let (m, n) = p.clone().instantiate(n);
                Ok((Substitution::new(), m, n))
            }
            None => Err(TypeError::UnknownVariable(v.clone())),
        },
        Term::App(t1, t2) => {
            let (s1, m1, n) = w(env, t1, data_cons, n)?;
            let (s2, m2, n) = w(&env.clone().substitute(&s1), t2, data_cons, n)?;
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
            let (s, m, n) = w(env, t, data_cons, n + 1)?;
            Ok((s.clone(), MonoType::function(beta, m).substitute(&s), n))
        }
        Term::Let(b, t1, t2) => {
            let (s1, m1, n) = w(env, t1, data_cons, n)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), m1.generalise(&env));
            }
            let (s2, m2, n) = w(&env, t2, data_cons, n)?;
            Ok((s1.combine(&s2), m2, n))
        }
        Term::Fix(f, b, t) => {
            let beta = MonoType::Var(n.into());
            let env = env.clone() + (f.clone(), beta.clone().into());
            let (s1, m1, n) = w(&env, &Term::Abs(b.clone(), t.clone()), data_cons, n + 1)?;
            let s2 = unify(beta.substitute(&s1), m1.clone())?;
            Ok((s1.combine(&s2), m1.substitute(&s2), n))
        }
        Term::Match(_, _) => todo!(),
    }
}

pub fn m(
    env: &Environment<PolyType>,
    term: &Term,
    data_cons: &Environment<DataConstructor>,
    mono: MonoType,
    n: usize,
) -> Result<(Substitution, usize)> {
    match term {
        Term::Lit(lit) => {
            let m = MonoType::Con(match lit {
                Literal::Unit => "Unit".to_owned(),
                Literal::Int(_) => "Int".to_owned(),
            });
            unify(mono, m).map(|s| (s, n))
        }
        Term::Var(v) => match env.get(v) {
            Some(p) => {
                let (m2, n) = p.clone().instantiate(n);
                unify(mono, m2).map(|s| (s, n))
            }
            None => Err(TypeError::UnknownVariable(v.clone())),
        },
        Term::App(t1, t2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, t1, data_cons, MonoType::function(beta.clone(), mono), n + 1)?;
            let env = env.clone().substitute(&s1);
            let (s2, n) = m(&env, t2, data_cons, beta.substitute(&s1), n)?;
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
            let (s2, n) = m(&env, t, data_cons, beta2.substitute(&s1), n + 2)?;
            Ok((s1.combine(&s2), n))
        }
        Term::Let(b, t1, t2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(env, t1, data_cons, beta.clone(), n + 1)?;
            let mut env = env.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                env += (x.clone(), beta.substitute(&s1).generalise(&env));
            }
            let (s2, n) = m(&env, t2, data_cons, mono.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Term::Fix(f, x, t) => {
            let env = env.clone() + (f.clone(), mono.clone().into());
            m(&env, &Term::Abs(x.clone(), t.clone()), data_cons, mono, n)
        }
        Term::Match(t, arms) => {
            let beta = MonoType::Var(n.into());
            let (mut s, mut n) = m(env, t, data_cons, beta.clone(), n + 1)?;
            for (pat, t) in arms {
                let data_con = data_cons
                    .get(&pat.constructor)
                    .ok_or(TypeError::UnknownDataConstructor(pat.constructor.clone()))?;

                s.combine_mut(&unify(
                    beta.clone().substitute(&s),
                    data_con.return_type.clone().substitute(&s),
                )?);

                if pat.bindings.len() != data_con.params.len() {
                    return Err(TypeError::IncorrectPatternParameters(
                        data_con.name.clone(),
                        data_con.params.len(),
                        pat.bindings.len(),
                    ));
                }

                let mut env = env.clone().substitute(&s);
                for (b, m) in pat.bindings.iter().zip(&data_con.params) {
                    if let Binding::Var(x) = b {
                        env += (x.clone(), m.clone().into());
                    }
                }

                let (s2, n2) = m(&env, t, data_cons, mono.clone().substitute(&s), n)?;
                n = n2;
                s.combine_mut(&s2);
            }

            check_match_exhaustive(&beta.substitute(&s), data_cons, arms)?;
            Ok((s, n))
        }
    }
}

fn check_match_exhaustive(
    m: &MonoType,
    data_cons: &Environment<DataConstructor>,
    arms: &[(Pattern, Term)],
) -> Result<()> {
    let needed_data_cons = data_cons
        .iter()
        .filter_map(|(name, data_con)| unify(m.clone(), data_con.return_type.clone()).ok().map(|_| name))
        .collect::<HashSet<_>>();
    let missing_data_cons = &needed_data_cons - &arms.iter().map(|(pat, _)| &pat.constructor).collect();
    if missing_data_cons.is_empty() {
        Ok(())
    } else {
        Err(TypeError::InexhausiveMatch(
            missing_data_cons.into_iter().cloned().collect(),
        ))
    }
}
