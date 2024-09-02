use std::ops::{Add, AddAssign};
use std::{collections::HashMap, fmt::Display};

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum TypeVariable {
    UserDefined(String),
    Inferred(usize),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TypeConstructor {
    Unit,
    Bool,
    Int,
    Function(Box<MonoType>, Box<MonoType>),
    List(Box<MonoType>),
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

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Context {
    pub map: HashMap<String, PolyType>,
}

#[derive(Clone, Debug)]
pub enum TypeError {
    UnknownVariable(String),
    InfiniteType(TypeVariable, MonoType),
    ConstructorConflict(TypeConstructor, TypeConstructor),
}

impl Display for TypeVariable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeVariable::UserDefined(s) => s.fmt(f),
            TypeVariable::Inferred(n) => write!(f, "τ{n}"),
        }
    }
}

impl Display for TypeConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TypeConstructor::*;
        match self {
            Unit => "Unit".fmt(f),
            Int => "Int".fmt(f),
            Bool => "Bool".fmt(f),
            Function(l, r) => match **l {
                MonoType::Con(Function(_, _)) => write!(f, "({l}) → {r}"),
                MonoType::Var(_) | MonoType::Con(_) => write!(f, "{l} → {r}"),
            },
            List(m) => match **m {
                MonoType::Con(Function(_, _)) => write!(f, "List ({m})"),
                MonoType::Var(_) | MonoType::Con(_) => write!(f, "List {m}"),
            },
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
            TypeError::UnknownVariable(x) => write!(f, "variable '{x}' is not defined"),
            TypeError::InfiniteType(t, m) => write!(f, "cannot construct infinite type {t} = {m}"),
            TypeError::ConstructorConflict(c1, c2) => write!(f, "expected type {c1}, found {c2}"),
        }
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

impl<P: Into<PolyType>> AddAssign<(String, P)> for Context {
    fn add_assign(&mut self, (name, p): (String, P)) {
        self.map.insert(name, p.into());
    }
}

impl<P: Into<PolyType>> Add<(String, P)> for Context {
    type Output = Self;

    fn add(mut self, rhs: (String, P)) -> Self::Output {
        self += rhs;
        self
    }
}

impl Context {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
}

impl MonoType {
    pub fn traverse(&mut self, f: &mut impl FnMut(&mut MonoType)) {
        use TypeConstructor::*;
        f(self);
        match self {
            MonoType::Var(_) | MonoType::Con(Unit | Bool | Int) => {}
            MonoType::Con(List(m)) => m.traverse(f),
            MonoType::Con(Function(l, r)) => {
                l.traverse(f);
                r.traverse(f);
            }
        }
    }
}
