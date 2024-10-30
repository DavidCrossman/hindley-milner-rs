use crate::model::term::{Binding, Pattern, Term};
use crate::model::Environment;
use crate::program::{Expression, Value};
use std::fmt::Display;
use thiserror::Error;

#[derive(Debug, Clone)]
pub enum Frame {
    HApp(Term, Environment<Value>),
    AppH(Value),
    HMatch(Vec<(Pattern, Term)>, Environment<Value>),
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("variable '{0}' is not defined")]
    UnknownVariable(String),
    #[error("invalid state: expression = {0}; environment = {1}; next frame = {}",
        .2.as_ref().map_or("None".to_owned(), ToString::to_string))]
    InvalidState(Expression, Environment<Value>, Option<Frame>),
    #[error("error in built-in function '{0}'")]
    BuiltIn(String, #[source] anyhow::Error),
    #[error("could not find arm in match expression for constructor '{0}'")]
    MissingMatchArm(String),
    #[error("cannot match with non-data value: {0}")]
    InvalidMatchValue(Value),
}

pub type Result<T> = std::result::Result<T, EvalError>;

type State = (Expression, Environment<Value>, Vec<Frame>);

impl Display for Frame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::HApp(t, env) => write!(f, "HApp ({t}) {env}"),
            Self::AppH(v) => write!(f, "AppH ({v})"),
            Self::HMatch(arms, env) => {
                let arms = arms.iter().map(|(p, t)| format!("{p} ⇒ {t}")).collect::<Vec<_>>();
                write!(f, "HMatch [{}] {env}", arms.join(","))
            }
        }
    }
}

pub fn eval(expr: Expression, global: &Environment<Expression>) -> Result<Value> {
    let mut s = (expr, Environment::new(), Vec::new());
    loop {
        s = match eval1(s, global)? {
            (Expression::Value(v), _, k) if k.is_empty() => break Ok(v),
            s => s,
        }
    }
}

fn eval1((expr, mut env, mut k): State, global: &Environment<Expression>) -> Result<State> {
    use self::Term::{Abs, App, Fix, Let, Lit, Match, Var};
    use self::Value::{BuiltIn, Closure, FixClosure};
    use Expression::{Term, Value};
    match expr {
        Term(Lit(lit)) => Ok((Value(lit.into()), env, k)),
        Term(Var(x)) => match env.remove(&x) {
            Some(v) => Ok((Value(v), env, k)),
            None => (global.get(&x))
                .map(|e| (e.clone(), Environment::new(), k))
                .ok_or(EvalError::UnknownVariable(x)),
        },
        Term(Abs(b, t)) => Ok((Value(Closure(b, *t, env)), Environment::new(), k)),
        Term(Fix(f, b, t)) => Ok((Value(FixClosure(f, b, *t, env)), Environment::new(), k)),
        Term(App(t1, t2)) => {
            k.push(Frame::HApp(*t2, env.clone()));
            Ok((Term(*t1), env, k))
        }
        Term(Let(x, t1, t2)) => Ok((Term(App(Box::new(Abs(x, t2)), t1)), env, k)),
        Term(Match(t, arms)) => {
            k.push(Frame::HMatch(arms, env.clone()));
            Ok((Term(*t), env, k))
        }
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
            Some(f @ Frame::AppH(_)) => Err(EvalError::InvalidState(Value(v), env, Some(f))),
            Some(Frame::HMatch(arms, mut env)) => {
                let self::Value::Data(name, values) = v else {
                    return Err(EvalError::InvalidMatchValue(v));
                };
                let Some((pat, t)) = arms.into_iter().find(|(pat, _)| pat.constructor == name) else {
                    return Err(EvalError::MissingMatchArm(name));
                };
                for (b, v) in pat.bindings.into_iter().zip(values) {
                    if let Binding::Var(x) = b {
                        env += (x, (*v).clone());
                    }
                }
                Ok((Term(t), env, k))
            }
            None => Ok((Value(v), env, k)),
        },
    }
}
