use crate::environment::Environment;
use crate::expression::Literal;
use crate::interpreter::{self, EvalError, Value};
use std::sync::{Arc, LazyLock};
use thiserror::Error;

#[derive(Clone)]
pub struct BuiltInFn {
    name: String,
    fun: Arc<dyn Fn(Value) -> anyhow::Result<Value> + Send + Sync>,
}

#[derive(Clone, Error, Debug)]
#[error("failed to convert value to type {type_name}")]
pub struct ValueConversionError {
    type_name: String,
}

impl std::fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BuiltInFn").field("name", &self.name).finish()
    }
}

impl std::fmt::Display for BuiltInFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.name.fmt(f)
    }
}

impl BuiltInFn {
    pub fn new(
        name: impl AsRef<str>,
        fun: impl Fn(Value) -> anyhow::Result<Value> + Send + Sync + 'static,
    ) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            fun: Arc::new(fun),
        }
    }

    pub fn apply(&self, value: Value) -> interpreter::Result<Value> {
        (self.fun)(value).map_err(|e| EvalError::BuiltIn(self.name.clone(), e))
    }

    pub fn make_unary<T, V>(
        name: impl AsRef<str>,
        f: impl Fn(T) -> anyhow::Result<V> + Send + Sync + 'static,
    ) -> Self
    where
        V: Into<Value>,
        Value: TryInto<T, Error = ValueConversionError>,
    {
        Self::new(name, move |x| Ok(f(x.try_into()?)?.into()))
    }

    pub fn make_binary<T, U, V>(
        name: impl AsRef<str>,
        f: impl Fn(T, U) -> anyhow::Result<V> + Copy + Send + Sync + 'static,
    ) -> Self
    where
        T: Clone + Send + Sync + 'static,
        V: Into<Value>,
        Value: TryInto<T, Error = ValueConversionError> + TryInto<U, Error = ValueConversionError>,
    {
        let name = name.as_ref().to_owned();
        Self::new(name.clone(), move |x| {
            let x: T = x.try_into()?;
            Ok(Value::BuiltIn(Self::new(name.clone(), move |y| {
                Ok(f(x.clone(), y.try_into()?)?.into())
            })))
        })
    }

    pub fn make_data_constructor(name: String, arity: usize) -> Self {
        Self::make_data_impl(name, arity, Vec::new())
    }

    fn make_data_impl(name: String, arity: usize, values: Vec<Value>) -> Self {
        Self::new(name.clone(), move |v| {
            let mut values = values.clone();
            values.push(v);
            if values.len() >= arity {
                Ok(Value::Data(name.clone(), values))
            } else {
                Ok(Value::BuiltIn(Self::make_data_impl(name.clone(), arity, values)))
            }
        })
    }
}

impl ValueConversionError {
    pub fn new(type_name: impl AsRef<str>) -> Self {
        Self {
            type_name: type_name.as_ref().to_owned(),
        }
    }
}

impl TryFrom<Value> for i64 {
    type Error = ValueConversionError;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Lit(Literal::Int(n)) = value {
            Ok(n)
        } else {
            Err(ValueConversionError::new("i64"))
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Lit(Literal::Int(value))
    }
}

pub static BUILT_INS: LazyLock<Environment<BuiltInFn>> = LazyLock::new(|| {
    let built_ins = [
        BuiltInFn::make_binary("add", |x: i64, y: i64| Ok(x + y)),
        BuiltInFn::make_binary("sub", |x: i64, y: i64| Ok(x - y)),
        BuiltInFn::make_binary("mul", |x: i64, y: i64| Ok(x * y)),
        BuiltInFn::make_binary("div", |x: i64, y: i64| match y {
            0 => anyhow::bail!("division by zero"),
            y => Ok(x / y),
        }),
    ];
    built_ins.into_iter().map(|f| (f.name.clone(), f)).collect()
});
