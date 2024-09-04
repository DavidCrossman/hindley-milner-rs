use crate::type_checking::model::*;
use std::collections::{HashMap, HashSet};
use std::iter;

impl MonoType {
    pub fn vars(&self) -> impl Iterator<Item = &TypeVariable> {
        use TypeConstructor::*;
        let iter: Box<dyn Iterator<Item = _>> = match self {
            MonoType::Var(t) => Box::new(iter::once(t)),
            MonoType::Con(Unit | Bool | Int) => Box::new(iter::empty()),
            MonoType::Con(List(m)) => m.vars(),
            MonoType::Con(Function(l, r)) => Box::new(l.vars().chain(r.vars())),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut TypeVariable> {
        use TypeConstructor::*;
        let iter: Box<dyn Iterator<Item = _>> = match self {
            MonoType::Var(t) => Box::new(iter::once(t)),
            MonoType::Con(Unit | Bool | Int) => Box::new(iter::empty()),
            MonoType::Con(List(m)) => m.vars_mut(),
            MonoType::Con(Function(l, r)) => Box::new(l.vars_mut().chain(r.vars_mut())),
        };
        iter
    }

    pub fn free_vars(&self) -> HashSet<&TypeVariable> {
        self.vars().collect()
    }

    pub fn generalise(self, context: &Context) -> PolyType {
        PolyType {
            quantifiers: self
                .free_vars()
                .difference(&context.free_vars())
                .copied()
                .cloned()
                .collect(),
            mono: self,
        }
    }
}

impl PolyType {
    pub fn free_vars(&self) -> HashSet<&TypeVariable> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter())
    }

    pub fn instantiate(mut self, next_fresh: usize) -> (MonoType, usize) {
        let n = next_fresh + self.quantifiers.len();
        let mappings = self
            .quantifiers
            .into_iter()
            .zip((next_fresh..).map(TypeVariable::Inferred))
            .collect::<HashMap<_, _>>();
        self.mono.vars_mut().for_each(|t1| {
            if let Some(t2) = mappings.get(t1) {
                *t1 = t2.to_owned();
            }
        });
        (self.mono, n)
    }
}

impl Context {
    pub fn free_vars(&self) -> HashSet<&TypeVariable> {
        self.map.values().flat_map(PolyType::free_vars).collect()
    }
}
