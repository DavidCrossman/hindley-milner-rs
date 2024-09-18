use super::Variable;
use std::fmt::Display;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Kind {
    Type,
    Var(Variable),
    Arrow(Box<Kind>, Box<Kind>),
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Type => "Type".fmt(f),
            Self::Var(v) => v.fmt(f),
            Self::Arrow(l, r) => match l.as_ref() {
                Self::Type | Self::Var(_) => write!(f, "{l} → {r}"),
                Self::Arrow(..) => write!(f, "({l}) → {r}"),
            },
        }
    }
}

impl From<Variable> for Kind {
    fn from(value: Variable) -> Self {
        Self::Var(value)
    }
}
