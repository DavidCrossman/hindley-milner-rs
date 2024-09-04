use crate::environment::Environment;
use crate::expression::{Binding, Expression};
use crate::type_checking::model::{MonoType, PolyType, TypeVariable};
use std::{collections::HashSet, iter};

pub trait FreeVariable<V> {
    fn free_vars(&self) -> HashSet<&V>;
}

impl FreeVariable<TypeVariable> for MonoType {
    fn free_vars(&self) -> HashSet<&TypeVariable> {
        self.vars().collect()
    }
}

impl FreeVariable<TypeVariable> for PolyType {
    fn free_vars(&self) -> HashSet<&TypeVariable> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter())
    }
}

impl FreeVariable<String> for Expression {
    fn free_vars(&self) -> HashSet<&String> {
        use Expression::*;
        match self {
            Lit(_) => HashSet::new(),
            Var(v) => [v].into(),
            App(e1, e2) => e1.free_vars().union(&e2.free_vars()).copied().collect(),
            Abs(Binding::Var(x), e) => &e.free_vars() - &[x].into(),
            Abs(Binding::Discard, e) => e.free_vars(),
            Let(Binding::Var(x), e1, e2) => (&e2.free_vars() - &[x].into())
                .union(&e1.free_vars())
                .copied()
                .collect(),
            Let(Binding::Discard, e1, e2) => e2.free_vars().union(&e1.free_vars()).copied().collect(),
            Closure(Binding::Var(x), e, env) => &e.free_vars() - &env.names().chain(iter::once(x)).collect(),
            Closure(Binding::Discard, e, env) => &e.free_vars() - &env.names().collect(),
            Fix(f, Binding::Var(x), e) => &e.free_vars() - &[f, x].into(),
            Fix(f, Binding::Discard, e) => &e.free_vars() - &[f].into(),
        }
    }
}

impl<V: Eq + std::hash::Hash, T: FreeVariable<V>> FreeVariable<V> for Environment<T> {
    fn free_vars(&self) -> HashSet<&V> {
        self.values().flat_map(T::free_vars).collect()
    }
}
