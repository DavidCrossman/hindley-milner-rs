use super::{PolyType, Variable};
use crate::model::{Environment, FreeVariable};
use std::{fmt::Display, iter};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeConstructor {
    Function,
    Named(String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MonoType {
    Var(Variable),
    Con(TypeConstructor),
    App(Box<MonoType>, Box<MonoType>),
}

enum DisplayMonoType {
    Name(String),
    PartialFunc(Box<DisplayMonoType>),
    Func(Box<DisplayMonoType>, Box<DisplayMonoType>),
    App(Box<DisplayMonoType>, Box<DisplayMonoType>),
}

impl From<String> for TypeConstructor {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl From<Variable> for MonoType {
    fn from(value: Variable) -> Self {
        Self::Var(value)
    }
}

impl From<TypeConstructor> for MonoType {
    fn from(value: TypeConstructor) -> Self {
        Self::Con(value)
    }
}

impl Display for TypeConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Function => "→".fmt(f),
            Self::Named(s) => s.fmt(f),
        }
    }
}

impl Display for DisplayMonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(s) => s.fmt(f),
            Self::PartialFunc(m) => write!(f, "({m} {})", TypeConstructor::Function),
            Self::Func(l, r) => match l.as_ref() {
                Self::Name(_) | Self::App(..) | Self::PartialFunc(_) => {
                    write!(f, "{l} {} {r}", TypeConstructor::Function)
                }
                Self::Func(..) => write!(f, "({l}) {} {r}", TypeConstructor::Function),
            },
            Self::App(m1, m2) => match m1.as_ref() {
                Self::Func(..) => match m2.as_ref() {
                    Self::Name(_) => write!(f, "({m1}) {m2}"),
                    Self::Func(..) | Self::App(..) | Self::PartialFunc(_) => write!(f, "({m1}) ({m2})"),
                },
                Self::Name(_) | Self::App(..) | Self::PartialFunc(_) => match m2.as_ref() {
                    Self::Name(_) => write!(f, "{m1} {m2}"),
                    Self::Func(..) | Self::App(..) | Self::PartialFunc(_) => write!(f, "{m1} ({m2})"),
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
            Box::new(Self::App(
                Box::new(Self::Con(TypeConstructor::Function)),
                Box::new(m1.into()),
            )),
            Box::new(m2.into()),
        )
    }

    pub fn traverse(&mut self, f: &mut impl FnMut(&mut Self)) {
        f(self);
        match self {
            Self::Var(_) | Self::Con(_) => {}
            Self::App(m1, m2) => {
                m1.traverse(f);
                m2.traverse(f);
            }
        }
    }

    pub fn vars(&self) -> impl Iterator<Item = &Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(t) => Box::new(iter::once(t)),
            Self::Con(_) => Box::new(iter::empty()),
            Self::App(m1, m2) => Box::new(m1.vars().chain(m2.vars())),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut Variable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(t) => Box::new(iter::once(t)),
            Self::Con(_) => Box::new(iter::empty()),
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
}

impl DisplayMonoType {
    fn make(m: &MonoType) -> Self {
        use MonoType::*;
        match m {
            Var(v) => Self::Name(v.to_string()),
            Con(con @ TypeConstructor::Function) => Self::Name(format!("({con})")),
            Con(con) => Self::Name(con.to_string()),
            App(m1, m2) => match m1.as_ref() {
                App(f, m1) if matches!(**f, Con(TypeConstructor::Function)) => {
                    Self::Func(Box::new(Self::make(m1)), Box::new(Self::make(m2)))
                }
                Con(TypeConstructor::Function) => Self::PartialFunc(Box::new(Self::make(m2))),
                _ => Self::App(Box::new(Self::make(m1)), Box::new(Self::make(m2))),
            },
        }
    }
}
