use super::term::{Binding, Pattern, Term};
use super::typing::{MonoType, PolyType, Variable};
use super::Environment;
use std::collections::HashSet;

pub trait FreeVariable<V: ?Sized> {
    fn free_vars(&self) -> HashSet<&V>;
}

impl FreeVariable<Variable> for MonoType {
    fn free_vars(&self) -> HashSet<&Variable> {
        self.vars().collect()
    }
}

impl FreeVariable<Variable> for PolyType {
    fn free_vars(&self) -> HashSet<&Variable> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter())
    }
}

impl FreeVariable<str> for Pattern {
    fn free_vars(&self) -> HashSet<&str> {
        self.bindings
            .iter()
            .filter_map(|b| match b {
                Binding::Var(v) => Some(v.as_str()),
                Binding::Discard => None,
            })
            .collect()
    }
}

impl FreeVariable<str> for Term {
    fn free_vars(&self) -> HashSet<&str> {
        use Term::*;
        match self {
            Lit(_) => HashSet::new(),
            Var(v) => [v.as_str()].into(),
            App(t1, t2) => t1.free_vars().union(&t2.free_vars()).copied().collect(),
            Abs(Binding::Var(x), t) => &t.free_vars() - &[x.as_str()].into(),
            Abs(Binding::Discard, t) => t.free_vars(),
            Let(Binding::Var(x), t1, t2) => (&t2.free_vars() - &[x.as_str()].into())
                .union(&t1.free_vars())
                .copied()
                .collect(),
            Let(Binding::Discard, t1, t2) => t2.free_vars().union(&t1.free_vars()).copied().collect(),
            Fix(f, Binding::Var(x), t) => &t.free_vars() - &[f.as_str(), x.as_str()].into(),
            Fix(f, Binding::Discard, t) => &t.free_vars() - &[f.as_str()].into(),
            Match(t, arms) => (arms.iter())
                .flat_map(|(p, t)| &t.free_vars() - &p.free_vars())
                .chain(t.free_vars())
                .collect(),
        }
    }
}

impl<V: Eq + std::hash::Hash, T: FreeVariable<V>> FreeVariable<V> for Environment<T> {
    fn free_vars(&self) -> HashSet<&V> {
        self.values().flat_map(T::free_vars).collect()
    }
}
