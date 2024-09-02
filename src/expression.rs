use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter,
};

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
    Bool(bool),
    Nat(u64),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Expression {
    Lit(Literal),
    Var(String),
    App(Box<Expression>, Box<Expression>),
    Abs(String, Box<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
    Closure(String, Box<Expression>, Environment),
    Fix(String, String, Box<Expression>),
}

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Environment(pub HashMap<String, Expression>);

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
            self.0
                .iter()
                .map(|(k, v)| format!("{k}={v}"))
                .collect::<Vec<_>>()
                .join(",")
        )
    }
}

impl FromIterator<(String, Expression)> for Environment {
    fn from_iter<T: IntoIterator<Item = (String, Expression)>>(iter: T) -> Self {
        Self(HashMap::from_iter(iter))
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
        match self {
            Expression::Lit(_) => HashSet::new(),
            Expression::Var(v) => [v].into(),
            Expression::App(e1, e2) => e1.free_vars().union(&e2.free_vars()).copied().collect(),
            Expression::Abs(x, e) => &e.free_vars() - &[x].into(),
            Expression::Let(x, e1, e2) => (&e2.free_vars() - &[x].into())
                .union(&e1.free_vars())
                .copied()
                .collect(),
            Expression::Closure(x, e, env) => &e.free_vars() - &env.0.keys().chain(iter::once(x)).collect(),
            Expression::Fix(f, x, e) => &e.free_vars() - &[f, x].into(),
        }
    }
}

impl Environment {
    pub fn new() -> Self {
        Self(HashMap::new())
    }
}
