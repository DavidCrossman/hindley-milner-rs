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
                m1.substitute(&s2),
                TypeConstructor::Function(Box::new(m2), Box::new(beta.clone())).into(),
            )?;
            Ok((s1.combine(&s2.combine(&s3)), beta.substitute(&s3), n + 1))
        }
        Expression::Abs(x, e) => {
            let beta = MonoType::Var(TypeVariable::Inferred(n));
            let context = context.clone() + (x.clone(), beta.clone());
            let (s, m, n) = w(&context, e, n + 1)?;
            let m = MonoType::Con(TypeConstructor::Function(Box::new(beta), Box::new(m)));
            Ok((s.clone(), m.substitute(&s), n))
        }
        Expression::Let(x, e1, e2) => {
            let (s1, m1, n) = w(context, e1, n)?;
            let p = m1.generalise(&context.clone().substitute(&s1));
            let context = context.clone() + (x.clone(), p);
            let (s2, m2, n) = w(&context.substitute(&s1), e2, n)?;
            Ok((s1.combine(&s2), m2, n))
        }
    }
}
