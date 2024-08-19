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
        self.traverse(&mut |m1| {
            if let MonoType::Var(t) = m1 {
                if let Some(m2) = subst.get(t) {
                    *m1 = m2.clone();
                }
            }
        });
    }
}

impl Substitute for PolyType {
    fn substitute_mut(&mut self, subst: &Substitution) {
        let mut subst = subst.clone();
        subst.retain(|t, _| !self.quantifiers.contains(t));
        self.mono.substitute_mut(&subst);
    }
}

impl Substitute for Context {
    fn substitute_mut(&mut self, subst: &Substitution) {
        self.values_mut().for_each(|m| m.substitute_mut(subst));
    }
}
