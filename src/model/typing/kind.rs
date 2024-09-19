use super::Variable;
use std::{fmt::Display, iter};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Var(Variable),
    Arrow(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type => "Type".fmt(f),
            Self::Var(v) => v.fmt(f),
            Self::Arrow(l, r) => match l.as_ref() {
                Self::Type | Self::Var(_) => write!(f, "{l} → {r}"),
                Self::Arrow(..) => write!(f, "({l}) → {r}"),
            },
        }
    }
}

impl From<Variable> for Kind {
    fn from(value: Variable) -> Self {
        Self::Var(value)
    }
}

impl Kind {
    pub fn arrow(k1: impl Into<Self>, k2: impl Into<Self>) -> Self {
        Self::Arrow(Box::new(k1.into()), Box::new(k2.into()))
    }

    pub fn traverse(&mut self, f: &mut impl FnMut(&mut Self)) {
        f(self);
        match self {
            Self::Type | Self::Var(_) => {}
            Self::Arrow(k1, k2) => {
                k1.traverse(f);
                k2.traverse(f);
            }
        }
    }

    pub fn vars(&self) -> impl Iterator<Item = &Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Type => Box::new(iter::empty()),
            Self::Var(v) => Box::new(iter::once(v)),
            Self::Arrow(k1, k2) => Box::new(k1.vars().chain(k2.vars())),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Type => Box::new(iter::empty()),
            Self::Var(v) => Box::new(iter::once(v)),
            Self::Arrow(k1, k2) => Box::new(k1.vars_mut().chain(k2.vars_mut())),
        };
        iter
    }
}
