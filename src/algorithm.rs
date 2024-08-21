use crate::{model::*, parser::*, substitution::*, unification::*};

pub fn w(
    context: &Context,
    expr: &Expression,
    n: usize,
) -> Result<(Substitution, MonoType, usize), TypeError> {
    match expr {
        Expression::Lit(lit) => Ok((
            Substitution::new(),
            MonoType::Con(match lit {
                Literal::Bool(_) => TypeConstructor::Bool,
                Literal::Nat(_) => TypeConstructor::Int,
            }),
            n,
        )),
        Expression::Var(x) => match context.env.get(x) {
            Some(p) => Ok((
                Substitution::new(),
                p.clone().instantiate(n),
                n + p.quantifiers.len(),
            )),
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let (s1, m1, n) = w(context, e1, n)?;
            let (s2, m2, n) = w(&context.clone().substitute(&s1), e2, n)?;
            let beta = MonoType::Var(TypeVariable::Inferred(n));
            let s3 = unify(
                TypeConstructor::Function(Box::new(m2), Box::new(beta.clone())).into(),
                m1.substitute(&s2),
            )?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
        Expression::Abs(x, e) => {
            let beta = MonoType::Var(TypeVariable::Inferred(n));
            let (s, m, n) = w(&(context.clone() + (x.clone(), beta.clone())), e, n + 1)?;
            let m = MonoType::Con(TypeConstructor::Function(Box::new(beta), Box::new(m)));
            Ok((s.clone(), m.substitute(&s), n))
        }
        Expression::Let(x, e1, e2) => {
            let (s1, m1, n) = w(context, e1, n)?;
            let context = context.clone().substitute(&s1);
            let p = m1.generalise(&context);
            let (s2, m2, n) = w(&(context + (x.clone(), p)), e2, n)?;
            Ok((s1.combine(&s2), m2, n))
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
                Literal::Bool(_) => TypeConstructor::Bool,
                Literal::Nat(_) => TypeConstructor::Int,
            }),
        )
        .map(|s| (s, n)),
        Expression::Var(x) => match context.env.get(x) {
            Some(p) => unify(t, p.clone().instantiate(n)).map(|s| (s, n + p.quantifiers.len())),
            None => Err(TypeError::UnknownVariable(x.clone())),
        },
        Expression::App(e1, e2) => {
            let beta = MonoType::Var(TypeVariable::Inferred(n));
            let t = TypeConstructor::Function(Box::new(beta.clone()), Box::new(t));
            let (s1, n) = m(context, e1, t.into(), n + 1)?;
            let (s2, n) = m(&context.clone().substitute(&s1), e2, beta.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Abs(x, e) => {
            let beta1 = MonoType::Var(TypeVariable::Inferred(n));
            let beta2 = MonoType::Var(TypeVariable::Inferred(n + 1));
            let t2 = TypeConstructor::Function(Box::new(beta1.clone()), Box::new(beta2.clone())).into();
            let s1 = unify(t, t2)?;
            let context = context.clone().substitute(&s1) + (x.clone(), beta1.substitute(&s1));
            let (s2, n) = m(&context, e, beta2.substitute(&s1), n + 2)?;
            Ok((s1.combine(&s2), n))
        }
        Expression::Let(x, e1, e2) => {
            let beta = MonoType::Var(TypeVariable::Inferred(n));
            let (s1, n) = m(context, e1, beta.clone(), n + 1)?;
            let context = context.clone().substitute(&s1);
            let p = beta.substitute(&s1).generalise(&context);
            let (s2, n) = m(&(context + (x.clone(), p)), e2, t.substitute(&s1), n)?;
            Ok((s1.combine(&s2), n))
        }
    }
}
