use crate::environment::Environment;
use crate::expression::Literal;
use crate::interpreter::{self, EvalError, Value};
use std::sync::{Arc, LazyLock};

#[derive(Clone)]
pub struct BuiltInFn {
    name: String,
    fun: Arc<dyn Fn(Value) -> anyhow::Result<Value> + Send + Sync>,
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
}

pub static BUILT_INS: LazyLock<Environment<BuiltInFn>> = LazyLock::new(|| {
    let mut env = Environment::new();
    env += (
        "add".to_owned(),
        BuiltInFn::new("add", move |x| {
            let Value::Lit(Literal::Int(x)) = x else {
                unimplemented!()
            };
            Ok(Value::BuiltIn(BuiltInFn::new(format!("add {x}"), move |y| {
                let Value::Lit(Literal::Int(y)) = y else {
                    unimplemented!()
                };
                Ok(Value::Lit(Literal::Int(x + y)))
            })))
        }),
    );
    env
});
