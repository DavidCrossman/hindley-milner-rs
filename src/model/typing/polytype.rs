use super::{MonoType, Variable};
use std::{collections::HashMap, fmt::Display};

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PolyType {
    pub quantifiers: Vec<Variable>,
    pub mono: MonoType,
}

impl From<MonoType> for PolyType {
    fn from(value: MonoType) -> Self {
        Self {
            quantifiers: Default::default(),
            mono: value,
        }
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for v in &self.quantifiers {
            write!(f, "∀{v}. ")?;
        }
        write!(f, "{}", self.mono)
    }
}

impl PolyType {
    pub fn new(mono: impl Into<MonoType>, quantifiers: impl IntoIterator<Item: Into<Variable>>) -> Self {
        Self {
            quantifiers: quantifiers.into_iter().map(Into::into).collect(),
            mono: mono.into(),
        }
    }

    pub fn instantiate(mut self, next_fresh: usize) -> (MonoType, usize) {
        let n = next_fresh + self.quantifiers.len();
        let mappings = (self.quantifiers.into_iter())
            .zip((next_fresh..).map(Variable::Inferred))
            .collect::<HashMap<_, _>>();
        self.mono.vars_mut().for_each(|v1| {
            if let Some(v2) = mappings.get(v1) {
                *v1 = v2.to_owned();
            }
        });
        (self.mono, n)
    }
}
