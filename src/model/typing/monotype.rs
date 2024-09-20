use super::{Kind, PolyType, Variable};
use crate::model::{Environment, FreeVariable};
use std::{fmt::Display, iter};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MonoType {
    Var(Variable),
    Con(String),
    Arrow,
    App(Box<MonoType>, Box<MonoType>),
}

enum DisplayMonoType {
    Name(String),
    PartialFunc(Box<DisplayMonoType>),
    Func(Box<DisplayMonoType>, Box<DisplayMonoType>),
    App(Box<DisplayMonoType>, Box<DisplayMonoType>),
}

impl From<Variable> for MonoType {
    fn from(value: Variable) -> Self {
        Self::Var(value)
    }
}

impl Display for DisplayMonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(s) => s.fmt(f),
            Self::PartialFunc(m) => write!(f, "({m} →)"),
            Self::Func(l, r) => match l.as_ref() {
                Self::Name(_) | Self::App(..) | Self::PartialFunc(_) => {
                    write!(f, "{l} → {r}")
                }
                Self::Func(..) => write!(f, "({l}) → {r}"),
            },
            Self::App(m1, m2) => match m1.as_ref() {
                Self::Func(..) => match m2.as_ref() {
                    Self::Name(_) | Self::PartialFunc(_) => write!(f, "({m1}) {m2}"),
                    Self::Func(..) | Self::App(..) => write!(f, "({m1}) ({m2})"),
                },
                Self::Name(_) | Self::App(..) | Self::PartialFunc(_) => match m2.as_ref() {
                    Self::Name(_) | Self::PartialFunc(_) => write!(f, "{m1} {m2}"),
                    Self::Func(..) | Self::App(..) => write!(f, "{m1} ({m2})"),
                },
            },
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DisplayMonoType::make(self).fmt(f)
    }
}

impl MonoType {
    pub fn function(m1: impl Into<Self>, m2: impl Into<Self>) -> Self {
        Self::App(
            Box::new(Self::App(Box::new(Self::Arrow), Box::new(m1.into()))),
            Box::new(m2.into()),
        )
    }

    pub fn traverse(&mut self, f: &mut impl FnMut(&mut Self)) {
        f(self);
        match self {
            Self::Var(_) | Self::Con(_) | Self::Arrow => {}
            Self::App(m1, m2) => {
                m1.traverse(f);
                m2.traverse(f);
            }
        }
    }

    pub fn vars(&self) -> impl Iterator<Item = &Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(v) => Box::new(iter::once(v)),
            Self::Arrow | Self::Con(_) => Box::new(iter::empty()),
            Self::App(m1, m2) => Box::new(m1.vars().chain(m2.vars())),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(v) => Box::new(iter::once(v)),
            Self::Arrow | Self::Con(_) => Box::new(iter::empty()),
            Self::App(m1, m2) => Box::new(m1.vars_mut().chain(m2.vars_mut())),
        };
        iter
    }

    pub fn generalise(self, env: &Environment<PolyType>) -> PolyType {
        let diff = &self.free_vars() - &env.free_vars();
        PolyType {
            quantifiers: diff.into_iter().cloned().collect(),
            mono: self,
        }
    }

    pub fn substitute_constructors(&mut self, type_constructors: &Environment<Kind>) {
        self.traverse(&mut |m| {
            if let MonoType::Var(v) = &m {
                if type_constructors.contains_name(&v.to_name()) {
                    *m = MonoType::Con(v.to_name());
                }
            }
        });
    }
}

impl DisplayMonoType {
    fn make(m: &MonoType) -> Self {
        use MonoType::*;
        match m {
            Var(v) => Self::Name(v.to_string()),
            Arrow => Self::Name("(→)".to_owned()),
            Con(name) => Self::Name(name.clone()),
            App(m1, m2) => match m1.as_ref() {
                App(f, m1) if matches!(f.as_ref(), Arrow) => {
                    Self::Func(Box::new(Self::make(m1)), Box::new(Self::make(m2)))
                }
                Arrow => Self::PartialFunc(Box::new(Self::make(m2))),
                _ => Self::App(Box::new(Self::make(m1)), Box::new(Self::make(m2))),
            },
        }
    }
}
