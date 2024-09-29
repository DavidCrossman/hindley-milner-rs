use std::fmt::Display;

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
    Int(i64),
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub enum Binding {
    Var(String),
    Discard,
}

#[derive(Hash, PartialEq, Eq, Clone, Debug)]
pub struct Pattern {
    pub constructor: String,
    pub bindings: Vec<Binding>,
}

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Term {
    Lit(Literal),
    Var(String),
    App(Box<Term>, Box<Term>),
    Abs(Binding, Box<Term>),
    Let(Binding, Box<Term>, Box<Term>),
    Fix(String, Binding, Box<Term>),
    Match(Box<Term>, Vec<(Pattern, Term)>),
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

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.bindings.is_empty() {
            self.constructor.fmt(f)
        } else {
            let bindings = self.bindings.iter().map(ToString::to_string).collect::<Vec<_>>();
            write!(f, "{} {}", self.constructor, bindings.join(" "))
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
                Lit(_) | Var(_) | App(..) => match t2.as_ref() {
                    Lit(_) | Var(_) => write!(f, "{t1} {t2}"),
                    App(..) | Abs(..) | Let(..) | Fix(..) | Match(..) => write!(f, "{t1} ({t2})"),
                },
                Abs(..) | Let(..) | Fix(..) | Match(..) => match t2.as_ref() {
                    Lit(_) | Var(_) | Abs(..) | Let(..) | Fix(..) | Match(..) => write!(f, "({t1}) {t2}"),
                    App(..) => write!(f, "({t1}) ({t2})"),
                },
            },
            Abs(x, t) => write!(f, "λ{x} → {t}"),
            Let(x, t1, t2) => write!(f, "let {x} = {t1} in {t2}"),
            Fix(fun, x, t) => write!(f, "fix {fun} λ{x} → {t}"),
            Match(term, arms) => {
                let arms = arms.iter().map(|(p, t)| format!("{p} ⇒ {t}")).collect::<Vec<_>>();
                write!(f, "match {term} with {}", arms.join("; "))
            }
        }
    }
}

impl Pattern {
    pub fn new(constructor: String, bindings: Vec<Binding>) -> Self {
        Self {
            constructor,
            bindings,
        }
    }
}
