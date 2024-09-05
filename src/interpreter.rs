use crate::environment::Environment;
use crate::expression::{Binding, Expression, Literal, Program};
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone)]
pub enum Value {
    Lit(Literal),
    Closure(Binding, Expression, Environment<Value>),
    FixClosure(String, Binding, Expression, Environment<Value>),
    BuiltIn(BuiltInFn),
}

#[derive(Clone)]
pub enum Control {
    Val(Value),
    Expr(Expression),
}

#[derive(Clone)]
pub enum Frame {
    HApp(Expression, Environment<Value>),
    AppH(Value),
}

type Continuation = Vec<Frame>;

type State = (Control, Environment<Value>, Continuation);

#[derive(Clone)]
pub enum EvalError {
    NoMain,
    UnknownVariable(String),
    InvalidState(State),
}

pub type Result<T> = std::result::Result<T, EvalError>;

pub type BuiltInFn = Rc<dyn Fn(Value) -> Result<Value>>;

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(lit) => lit.fmt(f),
            Value::Closure(b, e, env) => write!(f, "λ{env} {b} → {e}"),
            Value::FixClosure(x, b, e, env) => write!(f, "fix {x} λ{env} {b} → {e}"),
            Value::BuiltIn(_) => "λ ? → ?".fmt(f),
        }
    }
}

impl Display for Control {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Control::Val(v) => v.fmt(f),
            Control::Expr(e) => e.fmt(f),
        }
    }
}

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Frame::HApp(e, env) => write!(f, "HApp ({e}) {env}"),
            Frame::AppH(e) => write!(f, "AppH ({e})"),
        }
    }
}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::NoMain => write!(f, "no 'main' definition found"),
            EvalError::UnknownVariable(x) => write!(f, "variable '{x}' is not defined"),
            EvalError::InvalidState((e, env, k)) => {
                write!(
                    f,
                    "invalid state:\ncontrol: {e}\nenvironment: {env}\nnext frame: {}",
                    k.first().map_or("None".to_owned(), |f| f.to_string())
                )
            }
        }
    }
}

pub fn run(program: &Program, built_ins: &Environment<BuiltInFn>) -> Result<Value> {
    let (_, expr_main) = program
        .iter()
        .find(|(name, _)| name == "main")
        .ok_or(EvalError::NoMain)?;
    let global = program
        .iter()
        .filter(|(name, _)| name != "main")
        .cloned()
        .collect();
    eval(expr_main, &global, built_ins)
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
            Some(Frame::AppH(BuiltIn(fun))) => Ok((Val(fun(v)?), env, k)),
            Some(f @ Frame::AppH(_)) => {
                k.push(f);
                Err(EvalError::InvalidState((Val(v), env, k)))
            }
            None => Ok((Val(v), env, k)),
        },
    }
}
