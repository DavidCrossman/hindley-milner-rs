use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
    Int(i64),
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Binding {
    Var(String),
    Discard,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term {
    Lit(Literal),
    Var(String),
    App(Box<Term>, Box<Term>),
    Abs(Binding, Box<Term>),
    Let(Binding, Box<Term>, Box<Term>),
    Fix(String, Binding, Box<Term>),
}

impl From<String> for Binding {
    fn from(value: String) -> Self {
        Self::Var(value)
    }
}

impl From<Literal> for Term {
    fn from(value: Literal) -> Self {
        Self::Lit(value)
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => "()".fmt(f),
            Literal::Int(n) => n.fmt(f),
        }
    }
}

impl Display for Binding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Binding::Var(v) => v.fmt(f),
            Binding::Discard => "_".fmt(f),
        }
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Term::*;
        match self {
            Lit(lit) => lit.fmt(f),
            Var(x) => x.fmt(f),
            App(t1, t2) => match t1.as_ref() {
                Lit(_) | Var(_) => match t2.as_ref() {
                    Lit(_) | Var(_) => write!(f, "{t1} {t2}"),
                    App(..) | Abs(..) | Let(..) | Fix(..) => write!(f, "{t1} ({t2})"),
                },
                Abs(..) | Let(..) | Fix(..) => match t2.as_ref() {
                    Lit(_) | Var(_) | Abs(..) | Let(..) | Fix(..) => write!(f, "({t1}) {t2}"),
                    App(..) => write!(f, "({t1}) ({t2})"),
                },
                App(..) => match t2.as_ref() {
                    Lit(_) | Var(_) => write!(f, "{t1} {t2}"),
                    App(..) | Abs(..) | Let(..) | Fix(..) => write!(f, "{t1} ({t2})"),
                },
            },
            Abs(x, t) => write!(f, "λ{x} → {t}"),
            Let(x, t1, t2) => write!(f, "let {x} = {t1} in {t2}"),
            Fix(fun, x, t) => write!(f, "fix {fun} λ{x} → {t}"),
        }
    }
}
