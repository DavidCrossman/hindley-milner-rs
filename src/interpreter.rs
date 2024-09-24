use crate::model::term::{Binding, Term};
use crate::model::Environment;
use crate::program::{BuiltInFn, Expression, Value};
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Frame {
    HApp(Term, Environment<Value>),
    AppH(Value),
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("variable '{0}' is not defined")]
    UnknownVariable(String),
    #[error("invalid state: expression = {0}; environment = {1}; next frame = {2}")]
    InvalidState(Expression, Environment<Value>, Frame),
    #[error("error in built-in function '{0}'")]
    BuiltIn(String, #[source] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, EvalError>;

type State = (Expression, Environment<Value>, Vec<Frame>);

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HApp(t, env) => write!(f, "HApp ({t}) {env}"),
            Self::AppH(v) => write!(f, "AppH ({v})"),
        }
    }
}

pub fn eval(expr: Expression, global: &Environment<Expression>, built_ins: &[BuiltInFn]) -> Result<Value> {
    let mut s = (expr, Environment::new(), Vec::new());
    loop {
        s = match eval1(s, global, built_ins)? {
            (Expression::Value(v), _, k) if k.is_empty() => break Ok(v),
            s => s,
        }
    }
}

fn eval1(
    (expr, mut env, mut k): State,
    global: &Environment<Expression>,
    built_ins: &[BuiltInFn],
) -> Result<State> {
    use self::Term::{Abs, App, Fix, Let, Lit, Var};
    use self::Value::{BuiltIn, Closure, FixClosure};
    use Expression::{Term, Value};
    match expr {
        Term(Lit(lit)) => Ok((Value(lit.into()), env, k)),
        Term(Var(x)) => (env.remove(&x).map(Value))
            .or_else(|| global.get(&x).cloned())
            .or_else(|| (built_ins.iter().find(|f| f.name() == x)).map(|f| Value(f.clone().into())))
            .ok_or(EvalError::UnknownVariable(x))
            .map(|e| (e, env, k)),
        Term(Abs(b, t)) => Ok((Value(Closure(b, *t, env)), Environment::new(), k)),
        Term(Fix(f, b, t)) => Ok((Value(FixClosure(f, b, *t, env)), Environment::new(), k)),
        Term(App(t1, t2)) => {
            k.push(Frame::HApp(*t2, env.clone()));
            Ok((Term(*t1), env, k))
        }
        Term(Let(x, t1, t2)) => Ok((Term(App(Box::new(Abs(x, t2)), t1)), env, k)),
        Value(v) => match k.pop() {
            Some(Frame::HApp(t, env)) => {
                k.push(Frame::AppH(v));
                Ok((Term(t), env, k))
            }
            Some(Frame::AppH(Closure(b, t, mut env))) => {
                if let Binding::Var(x) = b {
                    env += (x, v);
                }
                Ok((Term(t), env, k))
            }
            Some(Frame::AppH(FixClosure(f, b, t, mut env))) => {
                if let Binding::Var(x) = &b {
                    env += (x.clone(), v);
                }
                env += (f.clone(), FixClosure(f, b, t.clone(), env.clone()));
                Ok((Term(t), env, k))
            }
            Some(Frame::AppH(BuiltIn(fun))) => Ok((Value(fun.apply(v)?), env, k)),
            Some(f @ Frame::AppH(_)) => Err(EvalError::InvalidState(Value(v), env, f)),
            None => Ok((Value(v), env, k)),
        },
    }
}
