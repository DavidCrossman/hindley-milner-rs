use crate::{model::*, parser::Expression};
use std::{collections::HashSet, iter};

pub trait FreeVariable<T> {
    fn free_vars(self) -> HashSet<T>;
}

impl<'a> FreeVariable<&'a TypeVariable> for &'a MonoType {
    fn free_vars(self) -> HashSet<&'a TypeVariable> {
        self.vars().collect()
    }
}

impl<'a> FreeVariable<&'a TypeVariable> for &'a PolyType {
    fn free_vars(self) -> HashSet<&'a TypeVariable> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter())
    }
}

impl<'a> FreeVariable<&'a TypeVariable> for &'a Context {
    fn free_vars(self) -> HashSet<&'a TypeVariable> {
        self.map.values().flat_map(FreeVariable::free_vars).collect()
    }
}

impl<'a> FreeVariable<&'a String> for &'a Expression {
    fn free_vars(self) -> HashSet<&'a String> {
        match self {
            Expression::Lit(_) => HashSet::new(),
            Expression::Var(v) => [v].into(),
            Expression::App(e1, e2) => e1.free_vars().union(&e2.free_vars()).copied().collect(),
            Expression::Abs(x, e) => &e.free_vars() - &[x].into(),
            Expression::Let(x, e1, e2) => (&e2.free_vars() - &[x].into())
                .union(&e1.free_vars())
                .copied()
                .collect(),
            Expression::Closure(x, e, env) => &e.free_vars() - &env.0.keys().chain(iter::once(x)).collect(),
            Expression::Fix(f, x, e) => &e.free_vars() - &[f, x].into(),
        }
    }
}
