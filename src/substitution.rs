use crate::model::*;
use std::{
    collections::hash_map::{self, HashMap},
    fmt::Display,
};

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Substitution {
    map: HashMap<TypeVariable, MonoType>,
}

impl<'a> IntoIterator for &'a Substitution {
    type Item = (&'a TypeVariable, &'a MonoType);
    type IntoIter = hash_map::Iter<'a, TypeVariable, MonoType>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.iter()
    }
}

impl IntoIterator for Substitution {
    type Item = (TypeVariable, MonoType);
    type IntoIter = hash_map::IntoIter<TypeVariable, MonoType>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl FromIterator<(TypeVariable, MonoType)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (TypeVariable, MonoType)>>(iter: T) -> Self {
        Self {
            map: HashMap::from_iter(iter),
        }
    }
}

impl Display for Substitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{{}}}",
            self.into_iter()
                .map(|(x, m)| format!("{x} -> {m}"))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

impl Substitution {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn combine_mut(&mut self, other: &Self) {
        self.map.values_mut().for_each(|m| m.substitute_mut(other));
        self.map.extend(other.to_owned());
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
                if let Some(m2) = subst.map.get(t) {
                    *m1 = m2.clone();
                }
            }
        });
    }
}

impl Substitute for PolyType {
    fn substitute_mut(&mut self, subst: &Substitution) {
        let mut subst = subst.clone();
        subst.map.retain(|t, _| !self.quantifiers.contains(t));
        self.mono.substitute_mut(&subst);
    }
}

impl Substitute for Context {
    fn substitute_mut(&mut self, subst: &Substitution) {
        self.env.values_mut().for_each(|m| m.substitute_mut(subst));
    }
}
