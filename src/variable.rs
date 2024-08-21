use crate::model::*;
use std::collections::HashSet;

pub trait FreeVariable {
    fn free_vars(&self) -> HashSet<TypeVariable>;
}

impl FreeVariable for MonoType {
    fn free_vars(&self) -> HashSet<TypeVariable> {
        self.vars().map(|s| s.to_owned()).collect()
    }
}

impl FreeVariable for PolyType {
    fn free_vars(&self) -> HashSet<TypeVariable> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter().cloned())
    }
}

impl FreeVariable for Context {
    fn free_vars(&self) -> HashSet<TypeVariable> {
        self.env.values().flat_map(FreeVariable::free_vars).collect()
    }
}
