use crate::model::*;
use derive_more::derive::{Deref, DerefMut};
use std::collections::HashMap;

#[derive(Default, PartialEq, Eq, Deref, DerefMut, Clone)]
pub struct Substitution(HashMap<String, MonoType>);

impl<'a> IntoIterator for &'a Substitution {
    type Item = (&'a String, &'a MonoType);
    type IntoIter = std::collections::hash_map::Iter<'a, String, MonoType>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl IntoIterator for Substitution {
    type Item = (String, MonoType);
    type IntoIter = <HashMap<String, MonoType> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<(String, MonoType)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (String, MonoType)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
    }
}


impl Substitution {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    pub fn combine_mut(&mut self, other: &Self) {
        self.values_mut().for_each(|m| m.substitute_mut(other));
        self.extend(other.to_owned());
    }

    pub fn combine(mut self, other: &Self) -> Self {
        self.combine_mut(other);
        self
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
