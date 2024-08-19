use crate::variable::{self, FreeVariable};
use derive_more::derive::{Deref, DerefMut};
use std::{collections::HashMap, fmt::Display, iter};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeConstructor {
    Function(Box<MonoType>, Box<MonoType>),
    Int,
    Bool,
    List(Box<MonoType>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum MonoType {
    Var(String),
    Con(TypeConstructor),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct PolyType {
    pub quantifiers: Vec<String>,
    pub mono: MonoType,
}

#[derive(Default, PartialEq, Eq, Deref, DerefMut, Clone, Debug)]
pub struct Context(HashMap<String, PolyType>);

#[derive(Clone, Debug)]
pub enum TypeError {
    UnknownVariable(String),
    InfiniteType(String, MonoType),
    ConstructorConflict(TypeConstructor, TypeConstructor),
}

impl Display for TypeConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeConstructor::Function(l, r) => format!("({l} → {r})").fmt(f),
            TypeConstructor::Int => "Int".fmt(f),
            TypeConstructor::Bool => "Bool".fmt(f),
            TypeConstructor::List(m) => format!("List {m}").fmt(f),
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonoType::Var(t) => t.fmt(f),
            MonoType::Con(c) => c.fmt(f),
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

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnknownVariable(x) => write!(f, "variable {x} is not defined"),
            TypeError::InfiniteType(t, m) => write!(f, "cannot construct infinite type {t} = {m}"),
            TypeError::ConstructorConflict(c1, c2) => write!(f, "expected type {c1}, found {c2}"),
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}

impl MonoType {
    pub fn traverse(&mut self, f: &mut impl FnMut(&mut MonoType)) {
        f(self);
        match self {
            MonoType::Var(_) | MonoType::Con(TypeConstructor::Bool | TypeConstructor::Int) => {}
            MonoType::Con(TypeConstructor::List(m)) => m.traverse(f),
            MonoType::Con(TypeConstructor::Function(l, r)) => {
                l.traverse(f);
                r.traverse(f);
            }
        }
    }

    pub fn vars(&self) -> impl Iterator<Item = &str> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            MonoType::Var(t) => Box::new(iter::once(t.as_str())),
            MonoType::Con(TypeConstructor::List(m)) => m.vars(),
            MonoType::Con(TypeConstructor::Function(l, r)) => Box::new(l.vars().chain(r.vars())),
            MonoType::Con(TypeConstructor::Bool | TypeConstructor::Int) => Box::new(iter::empty()),
        };
        iter
    }

    pub fn vars_mut(&mut self) -> impl Iterator<Item = &mut String> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            MonoType::Var(t) => Box::new(iter::once(t)),
            MonoType::Con(TypeConstructor::List(m)) => m.vars_mut(),
            MonoType::Con(TypeConstructor::Function(l, r)) => {
                Box::new(l.vars_mut().chain(r.vars_mut()))
            }
            MonoType::Con(TypeConstructor::Bool | TypeConstructor::Int) => Box::new(iter::empty()),
        };
        iter
    }

    pub fn generalise(self, context: &Context) -> PolyType {
        PolyType {
            quantifiers: Vec::from_iter(&self.free_vars() - &context.free_vars()),
            mono: self,
        }
    }
}

impl PolyType {
    pub fn instantiate(mut self, context: &Context) -> MonoType {
        let mappings = self
            .quantifiers
            .into_iter()
            .zip(variable::fresh_vars(context.free_vars()))
            .collect::<HashMap<_, _>>();
        self.mono.vars_mut().for_each(|t1| {
            if let Some(t2) = mappings.get(t1) {
                *t1 = t2.to_owned();
            }
        });
        self.mono
    }
}
