use super::typing::{Kind, MonoType, PolyType, Variable};
use super::Environment;
use std::collections::{hash_map, HashMap};
use std::fmt::Display;

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Substitution<T> {
    map: HashMap<Variable, T>,
}

pub trait Substitute<T> {
    fn substitute_mut(&mut self, subst: &Substitution<T>);

    fn substitute(mut self, subst: &Substitution<T>) -> Self
    where
        Self: Sized,
    {
        self.substitute_mut(subst);
        self
    }
}

impl<T> IntoIterator for Substitution<T> {
    type Item = (Variable, T);
    type IntoIter = hash_map::IntoIter<Variable, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<T> FromIterator<(Variable, T)> for Substitution<T> {
    fn from_iter<I: IntoIterator<Item = (Variable, T)>>(iter: I) -> Self {
        Self {
            map: HashMap::from_iter(iter),
        }
    }
}

impl<T: Display> Display for Substitution<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mappings = (self.iter())
            .map(|(v, x)| format!("{v} -> {x}"))
            .collect::<Vec<_>>();
        write!(f, "{{{}}}", mappings.join(", "))
    }
}

impl<T> Substitution<T> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn iter(&self) -> hash_map::Iter<'_, Variable, T> {
        self.map.iter()
    }
}
impl<T: Clone + Substitute<T>> Substitution<T> {
    pub fn combine_mut(&mut self, other: &Self) {
        self.map.values_mut().for_each(|x| x.substitute_mut(other));
        self.map.extend(other.to_owned());
    }

    pub fn combine(mut self, other: &Self) -> Self {
        self.combine_mut(other);
        self
    }
}

impl Substitute<MonoType> for MonoType {
    fn substitute_mut(&mut self, subst: &Substitution<MonoType>) {
        self.traverse(&mut |m1| {
            if let Self::Var(v) = m1 {
                if let Some(m2) = subst.map.get(v) {
                    *m1 = m2.clone();
                }
            }
        });
    }
}

impl Substitute<MonoType> for PolyType {
    fn substitute_mut(&mut self, subst: &Substitution<MonoType>) {
        let mut subst = subst.clone();
        subst.map.retain(|v, _| !self.quantifiers.contains(v));
        self.mono.substitute_mut(&subst);
    }
}

impl Substitute<Kind> for Kind {
    fn substitute_mut(&mut self, subst: &Substitution<Kind>) {
        self.traverse(&mut |k1| {
            if let Self::Var(v) = k1 {
                if let Some(k2) = subst.map.get(v) {
                    *k1 = k2.clone();
                }
            }
        });
    }
}

impl<T, S: Substitute<T>> Substitute<T> for Environment<S> {
    fn substitute_mut(&mut self, subst: &Substitution<T>) {
        self.values_mut().for_each(|x| x.substitute_mut(subst));
    }
}
