use crate::model::term::{Binding, Literal, Term};
use crate::model::Environment;
use crate::program::BuiltInFn;
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Value {
    Lit(Literal),
    Closure(Binding, Term, Environment<Value>),
    FixClosure(String, Binding, Term, Environment<Value>),
    BuiltIn(BuiltInFn),
    Data(String, Vec<Value>),
}

#[derive(Debug, Clone)]
pub enum Control {
    Value(Value),
    Term(Term),
}

#[derive(Debug, Clone)]
pub enum Frame {
    HApp(Term, Environment<Value>),
    AppH(Value),
}

type Continuation = Vec<Frame>;

type State = (Control, Environment<Value>, Continuation);

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("variable '{0}' is not defined")]
    UnknownVariable(String),
    #[error("invalid state: control = {0}; environment = {1}; next frame = {2}")]
    InvalidState(Control, Environment<Value>, Frame),
    #[error("error in built-in function '{0}'")]
    BuiltIn(String, #[source] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, EvalError>;

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

impl Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Value(v) => v.fmt(f),
            Self::Term(t) => t.fmt(f),
        }
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HApp(t, env) => write!(f, "HApp ({t}) {env}"),
            Self::AppH(v) => write!(f, "AppH ({v})"),
        }
    }
}

pub fn eval(expr: &Term, global: &Environment<Control>, built_ins: &Environment<BuiltInFn>) -> Result<Value> {
    let mut s = (Control::Term(expr.clone()), Environment::new(), Vec::new());
    loop {
        s = match eval1(s, global, built_ins)? {
            (Control::Value(v), _, k) if k.is_empty() => break Ok(v),
            s => s,
        }
    }
}

fn eval1(
    (c, mut env, mut k): State,
    global: &Environment<Control>,
    built_ins: &Environment<BuiltInFn>,
) -> Result<State> {
    use self::Term::{Abs, App, Fix, Let, Lit as TermLit, Var};
    use self::Value::{BuiltIn, Closure, FixClosure, Lit as ValueLit};
    use Control::{Term, Value};
    match c {
        Term(TermLit(lit)) => Ok((Value(ValueLit(lit)), env, k)),
        Term(Var(x)) => (env.remove(&x).map(Value))
            .or_else(|| global.get(&x).cloned())
            .or_else(|| built_ins.get(&x).cloned().map(BuiltIn).map(Value))
            .ok_or(EvalError::UnknownVariable(x))
            .map(|c| (c, env, k)),
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
