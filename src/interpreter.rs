use crate::built_in::BuiltInFn;
use crate::environment::Environment;
use crate::expression::{Binding, Expression, Literal};
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Value {
    Lit(Literal),
    Closure(Binding, Expression, Environment<Value>),
    FixClosure(String, Binding, Expression, Environment<Value>),
    BuiltIn(BuiltInFn),
}

#[derive(Debug, Clone)]
pub enum Control {
    Val(Value),
    Expr(Expression),
}

#[derive(Debug, Clone)]
pub enum Frame {
    HApp(Expression, Environment<Value>),
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
            Self::Closure(b, e, env) => write!(f, "λ{env} {b} → {e}"),
            Self::FixClosure(x, b, e, env) => write!(f, "fix {x} λ{env} {b} → {e}"),
            Self::BuiltIn(fun) => fun.fmt(f),
        }
    }
}

impl Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Val(v) => v.fmt(f),
            Self::Expr(e) => e.fmt(f),
        }
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HApp(e, env) => write!(f, "HApp ({e}) {env}"),
            Self::AppH(e) => write!(f, "AppH ({e})"),
        }
    }
}

pub fn eval(
    expr: &Expression,
    global: &Environment<Expression>,
    built_ins: &Environment<BuiltInFn>,
) -> Result<Value> {
    let mut s = (Control::Expr(expr.clone()), Environment::new(), Vec::new());
    loop {
        s = match eval1(s, global, built_ins)? {
            (Control::Val(v), _, k) if k.is_empty() => break Ok(v),
            s => s,
        }
    }
}

fn eval1(
    (c, mut env, mut k): State,
    global: &Environment<Expression>,
    built_ins: &Environment<BuiltInFn>,
) -> Result<State> {
    use Control::{Expr, Val};
    use Expression::{Abs, App, Fix, Let, Lit as ExprLit, Var};
    use Value::{BuiltIn, Closure, FixClosure, Lit as ValLit};
    match c {
        Expr(ExprLit(lit)) => Ok((Val(ValLit(lit)), env, k)),
        Expr(Var(x)) => (env.remove(&x).map(Val))
            .or_else(|| global.get(&x).cloned().map(Expr))
            .or_else(|| built_ins.get(&x).cloned().map(BuiltIn).map(Val))
            .ok_or(EvalError::UnknownVariable(x))
            .map(|c| (c, env, k)),
        Expr(Abs(b, e)) => Ok((Val(Closure(b, *e, env)), Environment::new(), k)),
        Expr(Fix(f, b, e)) => Ok((Val(FixClosure(f, b, *e, env)), Environment::new(), k)),
        Expr(App(e1, e2)) => {
            k.push(Frame::HApp(*e2, env.clone()));
            Ok((Expr(*e1), env, k))
        }
        Expr(Let(x, e1, e2)) => Ok((Expr(App(Box::new(Abs(x, e2)), e1)), env, k)),
        Val(v) => match k.pop() {
            Some(Frame::HApp(e, env)) => {
                k.push(Frame::AppH(v));
                Ok((Expr(e), env, k))
            }
            Some(Frame::AppH(Closure(b, e, mut env))) => {
                if let Binding::Var(x) = b {
                    env += (x, v);
                }
                Ok((Expr(e), env, k))
            }
            Some(Frame::AppH(FixClosure(f, b, e, mut env))) => {
                if let Binding::Var(x) = &b {
                    env += (x.clone(), v);
                }
                env += (f.clone(), FixClosure(f, b, e.clone(), env.clone()));
                Ok((Expr(e), env, k))
            }
            Some(Frame::AppH(BuiltIn(fun))) => Ok((Val(fun.apply(v)?), env, k)),
            Some(f @ Frame::AppH(_)) => Err(EvalError::InvalidState(Val(v), env, f)),
            None => Ok((Val(v), env, k)),
        },
    }
}
