use crate::model::*;
use derive_more::derive::{Deref, DerefMut};
use std::collections::HashMap;

#[derive(Default, PartialEq, Eq, Deref, DerefMut, Clone)]
pub struct Substitution(HashMap<String, MonoType>);

impl IntoIterator for Substitution {
    type Item = (String, MonoType);

    type IntoIter = <HashMap<String, MonoType> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Substitution {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn combine(&mut self, other: &Self) {
        self.values_mut().for_each(|m| m.substitute_mut(other));
        self.extend(other.clone());
    }
}

pub trait Substitute {
    fn substitute_mut(&mut self, subst: &Substitution);

    fn substitute(mut self, subst: &Substitution) -> Self
    where
        Self: Sized,
    {
        self.substitute_mut(subst);
        self
    }
}

impl Substitute for MonoType {
    fn substitute_mut(&mut self, subst: &Substitution) {
        match self {
            MonoType::Var(s) if subst.contains_key(s) => *self = subst[s].clone(),
            MonoType::Con(TypeConstructor::List(m)) => m.substitute_mut(subst),
            MonoType::Con(TypeConstructor::Function(l, r)) => {
                l.substitute_mut(subst);
                r.substitute_mut(subst);
            }
            _ => {}
        }
    }
}

impl Substitute for PolyType {
    fn substitute_mut(&mut self, subst: &Substitution) {
        let mut subst = subst.clone();
        subst.retain(|t, _| !self.quantifiers.contains(t));
        self.mono.substitute_mut(&subst)
    }
}

impl Substitute for Context {
    fn substitute_mut(&mut self, subst: &Substitution) {
        self.values_mut().for_each(|m| m.substitute_mut(subst));
    }
}
