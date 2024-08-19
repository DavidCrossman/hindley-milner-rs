use crate::model::*;
use std::{collections::HashSet, iter};

pub fn fresh_var<T: AsRef<str>>(free_vars: impl IntoIterator<Item = T>) -> String {
    let vars = free_vars
        .into_iter()
        .map(|s| s.as_ref().to_owned())
        .collect::<Vec<_>>();
    for n in 0.. {
        let t = format!("t{n}");
        if !vars.contains(&t) {
            return t;
        }
    }
    unreachable!()
}

pub fn fresh_vars<T: AsRef<str>>(
    free_vars: impl IntoIterator<Item = T>,
) -> impl Iterator<Item = String> {
    let vars = free_vars
        .into_iter()
        .map(|s| s.as_ref().to_owned())
        .collect::<Vec<_>>();
    let mut i = 0;
    iter::from_fn(move || {
        for n in i.. {
            let t = format!("t{n}");
            if !vars.contains(&t) {
                i = n;
                return Some(t);
            }
        }
        None
    })
}

pub trait FreeVariable {
    fn free_vars(&self) -> HashSet<String>;
}

impl FreeVariable for MonoType {
    fn free_vars(&self) -> HashSet<String> {
        self.vars().map(|s| s.to_owned()).collect()
    }
}

impl FreeVariable for PolyType {
    fn free_vars(&self) -> HashSet<String> {
        &self.mono.free_vars() - &HashSet::from_iter(self.quantifiers.iter().cloned())
    }
}

impl FreeVariable for Context {
    fn free_vars(&self) -> HashSet<String> {
        self.values().flat_map(FreeVariable::free_vars).collect()
    }
}
