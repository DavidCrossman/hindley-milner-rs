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
pub enum PolyType {
    Mono(MonoType),
    Quantifier(String, Box<PolyType>),
}

#[derive(Default, PartialEq, Eq, Deref, DerefMut, Clone, Debug)]
pub struct Context(HashMap<String, PolyType>);

impl Display for TypeConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeConstructor::Function(a, b) => format!("({a} → {b})").fmt(f),
            TypeConstructor::Int => "Int".fmt(f),
            TypeConstructor::Bool => "Bool".fmt(f),
            TypeConstructor::List(a) => format!("List {a}").fmt(f),
        }
    }
}

impl Display for MonoType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MonoType::Var(a) => a.fmt(f),
            MonoType::Con(c) => c.fmt(f),
        }
    }
}

impl Display for PolyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PolyType::Mono(m) => m.fmt(f),
            PolyType::Quantifier(a, p) => format!("∀{a}. {p}").fmt(f),
        }
    }
}

impl Context {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}
