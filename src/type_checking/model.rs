use crate::{environment::Environment, free_variable::FreeVariable};
use std::{collections::HashMap, fmt::Display, iter};

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum TypeVariable {
    Named(String),
    Inferred(usize),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeConstructor {
    Unit,
    Int,
    Function(Box<MonoType>, Box<MonoType>),
    Custom(String, Vec<MonoType>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MonoType {
    Var(TypeVariable),
    Con(TypeConstructor),
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
        use TypeConstructor::*;
        match self {
            Unit => "Unit".fmt(f),
            Int => "Int".fmt(f),
            Function(l, r) => match **l {
                MonoType::Con(Function(..)) => write!(f, "({l}) → {r}"),
                MonoType::Var(_) | MonoType::Con(_) => write!(f, "{l} → {r}"),
            },
            Custom(s, params) => {
                if params.is_empty() {
                    return write!(f, "{s}");
                }
                let params = (params.iter())
                    .map(|m| match m {
                        MonoType::Con(Function(..)) => format!("({m})"),
                        MonoType::Con(Custom(_, params)) if !params.is_empty() => {
                            format!("({m})")
                        }
                        _ => m.to_string(),
                    })
                    .collect::<Vec<_>>();
                write!(f, "{s} {}", params.join(" "))
            }
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Var(t) => t.fmt(f),
            Self::Con(c) => c.fmt(f),
        }
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
    pub fn traverse(&mut self, f: &mut impl FnMut(&mut Self)) {
        use TypeConstructor::*;
        f(self);
        match self {
            Self::Var(_) | Self::Con(Unit | Int) => {}
            Self::Con(Function(l, r)) => {
                l.traverse(f);
                r.traverse(f);
            }
            Self::Con(Custom(_, params)) => params.iter_mut().for_each(|m| m.traverse(f)),
        }
    }

    pub fn vars(&self) -> impl Iterator<Item = &TypeVariable> {
        use TypeConstructor::*;
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(t) => Box::new(iter::once(t)),
            Self::Con(Unit | Int) => Box::new(iter::empty()),
            Self::Con(Function(l, r)) => Box::new(l.vars().chain(r.vars())),
            Self::Con(Custom(_, params)) => Box::new(params.iter().flat_map(Self::vars)),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut TypeVariable> {
        use TypeConstructor::*;
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Self::Var(t) => Box::new(iter::once(t)),
            Self::Con(Unit | Int) => Box::new(iter::empty()),
            Self::Con(Function(l, r)) => Box::new(l.vars_mut().chain(r.vars_mut())),
            Self::Con(Custom(_, params)) => Box::new(params.iter_mut().flat_map(Self::vars_mut)),
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
