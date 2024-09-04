use std::collections::{HashMap, HashSet};
use std::{fmt::Display, iter};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
    Bool(bool),
    Nat(u64),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Binding {
    Var(String),
    Discard,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Lit(Literal),
    Var(String),
    App(Box<Expression>, Box<Expression>),
    Abs(Binding, Box<Expression>),
    Let(Binding, Box<Expression>, Box<Expression>),
    Closure(Binding, Box<Expression>, Environment),
    Fix(String, Binding, Box<Expression>),
}

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Environment {
    pub map: HashMap<String, Expression>,
}

pub type Program = Vec<(String, Expression)>;

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => "()".fmt(f),
            Literal::Bool(b) => b.fmt(f),
            Literal::Nat(n) => n.fmt(f),
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::Var(x) => x.fmt(f),
            Binding::Discard => "_".fmt(f),
        }
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Expression::*;
        match self {
            Lit(lit) => lit.fmt(f),
            Var(x) => x.fmt(f),
            App(e1, e2) => match **e1 {
                Lit(_) | Var(_) => match **e2 {
                    Lit(_) | Var(_) => write!(f, "{e1} {e2}"),
                    App(..) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
                Abs(..) | Let(..) | Closure(..) | Fix(..) => match **e2 {
                    Lit(_) | Var(_) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "({e1}) {e2}"),
                    App(..) => write!(f, "({e1}) ({e2})"),
                },
                App(..) => match **e2 {
                    Lit(_) | Var(_) => write!(f, "{e1} {e2}"),
                    App(..) | Abs(..) | Let(..) | Closure(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
            },
            Abs(x, e) => write!(f, "λ{x} → {e}"),
            Let(x, e1, e2) => write!(f, "let {x} = {e1} in {e2}"),
            Closure(x, e, env) => write!(f, "λ{env} {x} → {e}"),
            Fix(fun, x, e) => write!(f, "fix {fun} λ{x} → {e}"),
        }
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{}]",
            self.map
                .iter()
                .map(|(k, v)| format!("{k}={v}"))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl From<String> for Binding {
    fn from(value: String) -> Self {
        Self::Var(value)
    }
}

impl FromIterator<(String, Expression)> for Environment {
    fn from_iter<T: IntoIterator<Item = (String, Expression)>>(iter: T) -> Self {
        Self {
            map: HashMap::from_iter(iter),
        }
    }
}

impl Expression {
    pub fn is_value(&self) -> bool {
        use Expression::*;
        match self {
            Lit(_) | Closure(..) => true,
            Var(_) | App(..) | Abs(..) | Let(..) | Fix(..) => false,
        }
    }

    pub fn free_vars(&self) -> HashSet<&String> {
        use Expression::*;
        match self {
            Lit(_) => HashSet::new(),
            Var(v) => [v].into(),
            App(e1, e2) => e1.free_vars().union(&e2.free_vars()).copied().collect(),
            Abs(Binding::Var(x), e) => &e.free_vars() - &[x].into(),
            Abs(Binding::Discard, e) => e.free_vars(),
            Let(Binding::Var(x), e1, e2) => (&e2.free_vars() - &[x].into())
                .union(&e1.free_vars())
                .copied()
                .collect(),
            Let(Binding::Discard, e1, e2) => e2.free_vars().union(&e1.free_vars()).copied().collect(),
            Closure(Binding::Var(x), e, env) => {
                &e.free_vars() - &env.map.keys().chain(iter::once(x)).collect()
            }
            Closure(Binding::Discard, e, env) => &e.free_vars() - &env.map.keys().collect(),
            Fix(f, Binding::Var(x), e) => &e.free_vars() - &[f, x].into(),
            Fix(f, Binding::Discard, e) => &e.free_vars() - &[f].into(),
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }
}
