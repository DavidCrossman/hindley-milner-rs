use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Literal {
    Unit,
    Bool(bool),
    Int(i64),
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
    Fix(String, Binding, Box<Expression>),
}

pub type Program = Vec<(String, Expression)>;

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Unit => "()".fmt(f),
            Literal::Bool(b) => b.fmt(f),
            Literal::Int(n) => n.fmt(f),
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
                    App(..) | Abs(..) | Let(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
                Abs(..) | Let(..) | Fix(..) => match **e2 {
                    Lit(_) | Var(_) | Abs(..) | Let(..) | Fix(..) => write!(f, "({e1}) {e2}"),
                    App(..) => write!(f, "({e1}) ({e2})"),
                },
                App(..) => match **e2 {
                    Lit(_) | Var(_) => write!(f, "{e1} {e2}"),
                    App(..) | Abs(..) | Let(..) | Fix(..) => write!(f, "{e1} ({e2})"),
                },
            },
            Abs(x, e) => write!(f, "λ{x} → {e}"),
            Let(x, e1, e2) => write!(f, "let {x} = {e1} in {e2}"),
            Fix(fun, x, e) => write!(f, "fix {fun} λ{x} → {e}"),
        }
    }
}

impl From<String> for Binding {
    fn from(value: String) -> Self {
        Self::Var(value)
    }
}
