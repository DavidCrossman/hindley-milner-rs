use super::{model::*, substitution::*, unification::unify};
use crate::expression::{Binding, Expression, Literal};

pub fn w(
    context: &Context,
    expr: &Expression,
    n: usize,
) -> Result<(Substitution, MonoType, usize), TypeError> {
    match expr {
        Expression::Lit(lit) => Ok((
            Substitution::new(),
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Unit,
                Literal::Bool(_) => TypeConstructor::Bool,
                Literal::Nat(_) => TypeConstructor::Int,
            }),
            n,
        )),
        Expression::Var(x) => match context.map.get(x) {
            Some(p) => {
                let (m, n) = p.clone().instantiate(n);
                Ok((Substitution::new(), m, n))
            }
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let (s1, m1, n) = w(context, e1, n)?;
            let (s2, m2, n) = w(&context.clone().substitute(&s1), e2, n)?;
            let beta = MonoType::Var(n.into());
            let s3 = unify(
                TypeConstructor::Function(Box::new(m2), Box::new(beta.clone())).into(),
                m1.substitute(&s2),
            )?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
        Expression::Abs(b, e) | Expression::Closure(b, e, _) => {
            let beta = MonoType::Var(n.into());
            let context = match b {
                Binding::Var(x) => &(context.clone() + (x.clone(), beta.clone())),
                Binding::Discard => context,
            };
            let (s, m, n) = w(context, e, n + 1)?;
            let m = MonoType::Con(TypeConstructor::Function(Box::new(beta), Box::new(m)));
            Ok((s.clone(), m.substitute(&s), n))
        }
        Expression::Let(b, e1, e2) => {
            let (s1, m1, n) = w(context, e1, n)?;
            let mut context = context.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                context += (x.clone(), m1.generalise(&context));
            }
            let (s2, m2, n) = w(&context, e2, n)?;
            Ok((s1.combine(&s2), m2, n))
        }
        Expression::Fix(f, b, e) => {
            let beta = MonoType::Var(n.into());
            let context = context.clone() + (f.clone(), beta.clone());
            let (s1, m1, n) = w(&context, &Expression::Abs(b.clone(), e.clone()), n + 1)?;
            let s2 = unify(beta.substitute(&s1), m1.clone())?;
            Ok((s1.combine(&s2), m1.substitute(&s2), n))
        }
    }
}

pub fn m(
    context: &Context,
    expr: &Expression,
    t: MonoType,
    n: usize,
) -> Result<(Substitution, usize), TypeError> {
    match expr {
        Expression::Lit(lit) => unify(
            t,
            MonoType::Con(match lit {
                Literal::Unit => TypeConstructor::Unit,
                Literal::Bool(_) => TypeConstructor::Bool,
                Literal::Nat(_) => TypeConstructor::Int,
            }),
        )
        .map(|s| (s, n)),
        Expression::Var(x) => match context.map.get(x) {
            Some(p) => {
                let (t2, n) = p.clone().instantiate(n);
                unify(t, t2).map(|s| (s, n))
            }
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let beta = MonoType::Var(n.into());
            let t = TypeConstructor::Function(Box::new(beta.clone()), Box::new(t));
            let (s1, n) = m(context, e1, t.into(), n + 1)?;
            let (s2, n) = m(&context.clone().substitute(&s1), e2, beta.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Abs(b, e) | Expression::Closure(b, e, _) => {
            let beta1 = MonoType::Var(n.into());
            let beta2 = MonoType::Var((n + 1).into());
            let t2 = TypeConstructor::Function(Box::new(beta1.clone()), Box::new(beta2.clone())).into();
            let s1 = unify(t, t2)?;
            let mut context = context.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                context += (x.clone(), beta1.substitute(&s1));
            }
            let (s2, n) = m(&context, e, beta2.substitute(&s1), n + 2)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Let(b, e1, e2) => {
            let beta = MonoType::Var(n.into());
            let (s1, n) = m(context, e1, beta.clone(), n + 1)?;
            let mut context = context.clone().substitute(&s1);
            if let Binding::Var(x) = b {
                context += (x.clone(), beta.substitute(&s1).generalise(&context));
            }
            let (s2, n) = m(&context, e2, t.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Fix(f, x, e) => {
            let context = context.clone() + (f.clone(), t.clone());
            m(&context, &Expression::Abs(x.clone(), e.clone()), t, n)
        }
    }
}
