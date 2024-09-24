use super::BuiltInFn;
use crate::model::term::{Binding, Literal, Term};
use crate::model::Environment;
use std::{fmt::Display, sync::Arc};
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Value {
    Lit(Literal),
    Closure(Binding, Term, Environment<Value>),
    FixClosure(String, Binding, Term, Environment<Value>),
    BuiltIn(BuiltInFn),
    Data(String, Vec<Arc<Value>>),
}

#[derive(Clone, Error, Debug)]
#[error("failed to convert value to type {type_name}")]
pub struct ValueConversionError {
    type_name: String,
}

impl TryFrom<Value> for () {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Lit(Literal::Unit) => Ok(()),
            _ => Err(ValueConversionError::new("()")),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Lit(Literal::Int(n)) => Ok(n),
            _ => Err(ValueConversionError::new("i64")),
        }
    }
}

impl From<()> for Value {
    fn from(_value: ()) -> Self {
        Self::Lit(Literal::Unit)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Lit(Literal::Int(value))
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        Self::Lit(value)
    }
}

impl From<BuiltInFn> for Value {
    fn from(value: BuiltInFn) -> Self {
        Self::BuiltIn(value)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lit(lit) => lit.fmt(f),
            Self::Closure(b, t, env) => write!(f, "λ{env} {b} → {t}"),
            Self::FixClosure(x, b, t, env) => write!(f, "fix {x} λ{env} {b} → {t}"),
            Self::BuiltIn(fun) => fun.fmt(f),
            Self::Data(name, values) => {
                let values = values.iter().map(ToString::to_string).collect::<Vec<_>>();
                write!(f, "{name} ⟨{}⟩", values.join(","))
            }
        }
    }
}

impl ValueConversionError {
    pub fn new(type_name: impl AsRef<str>) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
        }
    }
}
