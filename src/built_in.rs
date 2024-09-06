use crate::environment::Environment;
use crate::expression::Literal;
use crate::interpreter::{self, Value};
use std::sync::{Arc, LazyLock};

#[derive(Clone)]
pub struct BuiltInFn(Arc<dyn Fn(Value) -> interpreter::Result<Value> + Send + Sync>);

impl BuiltInFn {
    pub fn new(fun: impl Fn(Value) -> interpreter::Result<Value> + Send + Sync + 'static) -> Self {
        Self(Arc::new(fun))
    }

    pub fn apply(&self, value: Value) -> interpreter::Result<Value> {
        self.0(value)
    }
}

impl std::fmt::Debug for BuiltInFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("BuiltInFn").finish()
    }
}

pub static BUILT_INS: LazyLock<Environment<BuiltInFn>> = LazyLock::new(|| {
    let mut env = Environment::new();
    env += (
        "add".to_owned(),
        BuiltInFn::new(move |x| {
            let Value::Lit(Literal::Int(x)) = x else {
                unimplemented!()
            };
            Ok(Value::BuiltIn(BuiltInFn::new(move |y| {
                let Value::Lit(Literal::Int(y)) = y else {
                    unimplemented!()
                };
                Ok(Value::Lit(Literal::Int(x + y)))
            })))
        }),
    );
    env
});
