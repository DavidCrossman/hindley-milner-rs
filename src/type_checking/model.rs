use crate::{environment::Environment, free_variable::FreeVariable};
use std::{collections::HashMap, fmt::Display, iter};

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum TypeVariable {
    Named(String),
    Inferred(usize),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeConstructor {
    Function,
    Named(String),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MonoType {
    Var(TypeVariable),
    Con(TypeConstructor),
    App(Box<MonoType>, Box<MonoType>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PolyType {
    pub quantifiers: Vec<TypeVariable>,
    pub mono: MonoType,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Display for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named(s) => s.fmt(f),
            Self::Inferred(n) => write!(f, "τ{n}"),
        }
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

enum DisplayMonoType {
    Name(String),
    PartialFunc(Box<DisplayMonoType>),
    Func(Box<DisplayMonoType>, Box<DisplayMonoType>),
    App(Box<DisplayMonoType>, Box<DisplayMonoType>),
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

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for t in &self.quantifiers {
            write!(f, "∀{t}. ")?;
        }
        write!(f, "{}", self.mono)
    }
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type => "Type".fmt(f),
            Self::Arrow(l, r) => match **l {
                Self::Type => write!(f, "{l} → {r}"),
                Self::Arrow(..) => write!(f, "({l}) → {r}"),
            },
        }
    }
}

impl From<String> for TypeVariable {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl From<usize> for TypeVariable {
    fn from(value: usize) -> Self {
        Self::Inferred(value)
    }
}

impl From<String> for TypeConstructor {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl From<TypeVariable> for MonoType {
    fn from(value: TypeVariable) -> Self {
        Self::Var(value)
    }
}

impl From<TypeConstructor> for MonoType {
    fn from(value: TypeConstructor) -> Self {
        Self::Con(value)
    }
}

impl From<MonoType> for PolyType {
    fn from(value: MonoType) -> Self {
        Self {
            quantifiers: Default::default(),
            mono: value,
        }
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

    pub fn vars(&self) -> impl Iterator<Item = &TypeVariable> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(t) => Box::new(iter::once(t)),
            Self::Con(_) => Box::new(iter::empty()),
            Self::App(m1, m2) => Box::new(m1.vars().chain(m2.vars())),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut TypeVariable> {
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

impl PolyType {
    pub fn new(mono: impl Into<MonoType>, quantifiers: impl IntoIterator<Item: Into<TypeVariable>>) -> Self {
        Self {
            quantifiers: quantifiers.into_iter().map(Into::into).collect(),
            mono: mono.into(),
        }
    }

    pub fn instantiate(mut self, next_fresh: usize) -> (MonoType, usize) {
        let n = next_fresh + self.quantifiers.len();
        let mappings = (self.quantifiers.into_iter())
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
