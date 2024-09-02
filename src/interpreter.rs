use crate::expression::{Environment, Expression, Program};
use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Frame {
    HApp(Expression, Environment),
    AppH(Expression),
}

type Continuation = Vec<Frame>;

type State = (Expression, Environment, Continuation);

#[derive(Clone, Debug)]
pub enum EvalError {
    UnknownVariable(String),
    InvalidState(State),
    NoMain,
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
            EvalError::UnknownVariable(x) => write!(f, "variable {x} is not defined"),
            EvalError::InvalidState((e, env, k)) => {
                write!(
                    f,
                    "invalid state:\ncontrol: {e}\nenvironment: {env}\nnext frame: {}",
                    k.first().map_or("None".to_owned(), |f| f.to_string())
                )
            }
            EvalError::NoMain => write!(f, "no 'main' definition found"),
        }
    }
}

pub fn run(program: &Program) -> Result<Expression, EvalError> {
    let (_, expr_main) = program
        .iter()
        .find(|(name, _)| name == "main")
        .ok_or(EvalError::NoMain)?;
    let env = program
        .iter()
        .filter(|(name, _)| name != "main")
        .cloned()
        .collect();
    eval(env, expr_main)
}

pub fn eval(env: Environment, e: &Expression) -> Result<Expression, EvalError> {
    let mut s = (e.clone(), env, Vec::new());
    loop {
        let e1 = s.0.clone();
        s = eval1(s)?;
        let (e2, _, k) = &s;
        if &e1 == e2 && e2.is_value() && k.is_empty() {
            break Ok(e1);
        }
    }
}

fn eval1(s: State) -> Result<State, EvalError> {
    use Expression::*;
    match s {
        (Var(x), mut env, k) => match env.0.remove(&x).ok_or(EvalError::UnknownVariable(x))? {
            Closure(x, e, env) => Ok((Abs(x, e), env, k)),
            e => Ok((e, Environment::new(), k)),
        },
        (Abs(x, e), env, k) => Ok((Closure(x, e, env), Environment::new(), k)),
        (Fix(f, x, e), mut env, k) => {
            env.0.insert(f.clone(), Fix(f, x.clone(), e.clone()));
            Ok((Closure(x, e, env), Environment::new(), k))
        }
        (App(e1, e2), env, mut k) => {
            k.push(Frame::HApp(*e2, env.clone()));
            Ok((*e1, env, k))
        }
        (Let(x, e1, e2), env, k) => Ok((App(Box::new(Abs(x, e2)), e1), env, k)),
        (v, env, mut k) if v.is_value() => match k.pop() {
            Some(Frame::HApp(e, env)) => {
                k.push(Frame::AppH(v));
                Ok((e, env, k))
            }
            Some(Frame::AppH(Closure(x, e, mut env))) => {
                env.0.insert(x, v);
                Ok((*e, env, k))
            }
            Some(f) => {
                k.push(f);
                Err(EvalError::InvalidState((v, env, k)))
            }
            None => Ok((v, env, k)),
        },
        s => Err(EvalError::InvalidState(s)),
    }
}
