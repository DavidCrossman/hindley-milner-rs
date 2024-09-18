use super::Value;
use crate::model::term::Term;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum Expression {
    Value(Value),
    Term(Term),
}

impl From<Value> for Expression {
    fn from(value: Value) -> Self {
        Self::Value(value)
    }
}

impl From<Term> for Expression {
    fn from(value: Term) -> Self {
        Self::Term(value)
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(v) => v.fmt(f),
            Self::Term(t) => t.fmt(f),
        }
    }
}
