use std::fmt::Display;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
pub enum Variable {
    Named(String),
    Inferred(usize),
}

impl From<String> for Variable {
    fn from(value: String) -> Self {
        Self::Named(value)
    }
}

impl From<usize> for Variable {
    fn from(value: usize) -> Self {
        Self::Inferred(value)
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Named(s) => s.fmt(f),
            Self::Inferred(n) => write!(f, "τ{n}"),
        }
    }
}

impl Variable {
    pub fn to_name(&self) -> String {
        match self {
            Variable::Named(name) => name.clone(),
            Variable::Inferred(n) => format!("{n}"),
        }
    }
}
