use derive_more::derive::{Deref, DerefMut};
use std::{collections::HashMap, fmt::Display};

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

impl Context {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}
