mod built_in;
mod expression;
mod item;
mod value;

pub use built_in::BuiltInFn;
pub use expression::Expression;
pub use item::{DataConstructor, Item};
pub use value::{Value, ValueConversionError};

use crate::kind_inference::{self, KindError};
use crate::model::term::Term;
use crate::model::typing::{Kind, MonoType, PolyType, Variable};
use crate::model::{Environment, FreeVariable, Substitute, Substitution};
use crate::{interpreter, type_inference};
use std::collections::HashSet;
use thiserror::Error;

#[derive(Clone)]
pub struct Program {
    main: Term,
    global: Environment<Expression>,
    type_env: Environment<PolyType>,
    type_constructors: Environment<Kind>,
    built_ins: Environment<BuiltInFn>,
}

#[derive(Clone, Error, Debug)]
pub enum ProgramError {
    #[error("no 'main' definition found")]
    NoMain,
    #[error("missing definition for '{0}'")]
    MissingDefinition(String),
    #[error("unknown built-in function '{0}'")]
    UnknownBuiltIn(String),
    #[error("missing type signature for built-in function '{0}'")]
    BuiltInMissingSignature(String),
    #[error("duplicate definitions for '{0}'")]
    DuplicateDefinition(String),
    #[error("duplicate type signatures for '{0}'")]
    DuplicateSignature(String),
    #[error("unknown type variable '{0}'")]
    UnknownTypeVar(Variable),
    #[error("unused type variable '{0}'")]
    UnusedTypeVar(Variable),
    #[error("invalid type constructor '{0}'")]
    InvalidTypeConstructor(String, #[source] KindError),
}

pub type Result<T> = std::result::Result<T, ProgramError>;

impl Program {
    pub fn new(items: impl IntoIterator<Item = item::Item>) -> Result<Self> {
        let mut main = None;
        let mut global = Environment::new();
        let mut type_env = Environment::new();
        let mut kind_env = Environment::new();
        let mut built_ins = Environment::new();
        kind_env += ("Unit".to_owned(), Kind::Type);
        kind_env += ("Int".to_owned(), Kind::Type);
        for item in items {
            match item {
                Item::TermDefinition(name, term) => {
                    if global.contains_name(&name)
                        || built_ins.contains_name(&name)
                        || main.is_some() && name == "main"
                    {
                        return Err(ProgramError::DuplicateDefinition(name));
                    } else if name == "main" {
                        main = Some(term);
                    } else {
                        global += (name, Expression::Term(term));
                    }
                }
                Item::BuiltInDefinition(name) => {
                    if global.contains_name(&name) || built_ins.contains_name(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    }
                    let Some(fun) = built_in::BUILT_INS.get(&name).cloned() else {
                        return Err(ProgramError::UnknownBuiltIn(name));
                    };
                    built_ins += (name, fun);
                }
                Item::TypeDefinition(type_name, vars, sum) => {
                    let used_vars = (sum.iter())
                        .flat_map(|con| con.types.iter())
                        .fold(HashSet::new(), |vars, m| {
                            vars.union(&m.free_vars()).copied().collect()
                        });
                    if let Some(&v) = HashSet::from_iter(&vars).difference(&used_vars).next() {
                        return Err(ProgramError::UnusedTypeVar(v.clone()));
                    };
                    let kind = (vars.iter().rev()).fold(Kind::Type, |k, v| Kind::arrow(v.clone(), k));
                    kind_env += (type_name.clone(), kind.clone());
                    let mono = vars.iter().fold(MonoType::Con(type_name.clone()), |m, v| {
                        MonoType::App(Box::new(m), Box::new(MonoType::Var(v.clone())))
                    });
                    let mut subst = Substitution::new();
                    for DataConstructor { name, types } in sum {
                        let mut mono = mono.clone();
                        let expr = if types.is_empty() {
                            Expression::Value(Value::Data(name.clone(), Vec::new()))
                        } else {
                            let fun = BuiltInFn::make_data_constructor(name.clone(), types.len());
                            mono = (types.into_iter().rev()).fold(mono, |m1, m2| MonoType::function(m2, m1));
                            mono.substitute_constructors_mut(&kind_env);
                            Expression::Value(Value::BuiltIn(fun))
                        };

                        let p = PolyType::new(mono.clone(), vars.clone());
                        if let Some(&v) = p.free_vars().iter().next() {
                            return Err(ProgramError::UnknownTypeVar(v.clone()));
                        }
                        if type_env.contains_name(&name) {
                            return Err(ProgramError::DuplicateSignature(name));
                        }
                        if global.contains_name(&name) || built_ins.contains_name(&name) || name == "main" {
                            return Err(ProgramError::DuplicateDefinition(name));
                        }
                        global += (name.clone(), expr);
                        type_env += (name.clone(), p);

                        let mut env = kind_env.clone();
                        env.extend(vars.iter().map(|v| (v.to_name(), Kind::Var(v.clone()))));

                        mono.substitute_constructors_mut(&env);
                        let (s, _) = kind_inference::algorithm::m(&env, &mono, Kind::Type, 0)
                            .map_err(|e| ProgramError::InvalidTypeConstructor(name, e))?;

                        subst.combine_mut(&s);
                    }
                    let kind = kind.substitute(&subst);
                    let subst = kind.vars().cloned().map(|v| (v, Kind::Type)).collect();
                    kind_env += (type_name, kind.substitute(&subst));
                }
                Item::Declaration(name, m) => {
                    if type_env.contains_name(&name) {
                        return Err(ProgramError::DuplicateSignature(name));
                    }
                    type_env += (name, m.substitute_constructors(&kind_env).generalise(&type_env));
                }
            }
        }
        if let Some(name) = (type_env.names())
            .find(|name| !global.contains_name(name) && !built_ins.contains_name(name) && name != &"main")
        {
            return Err(ProgramError::MissingDefinition(name.clone()));
        }
        if let Some(name) = built_ins.names().find(|name| !type_env.contains_name(name)) {
            return Err(ProgramError::BuiltInMissingSignature(name.clone()));
        }
        Ok(Self {
            main: main.ok_or(ProgramError::NoMain)?,
            global,
            type_env,
            type_constructors: kind_env,
            built_ins,
        })
    }

    pub fn type_check(&mut self) -> type_inference::Result<()> {
        (self.global.iter())
            .filter_map(|(name, c)| match c {
                Expression::Term(t) => Some((name, t)),
                _ => None,
            })
            .chain(std::iter::once((&"main".to_owned(), &self.main)))
            .try_for_each(|(name, expr)| {
                let (mono, n) = match self.type_env.remove(name) {
                    Some(p) => p.instantiate(0),
                    None => (MonoType::Var(0.into()), 1),
                };
                let (s, _) = type_inference::algorithm::m(&self.type_env, expr, mono.clone(), n)?;
                let p = mono.substitute(&s).generalise(&self.type_env);
                self.type_env += (name.clone(), p);
                Ok(())
            })
    }

    pub fn kind_check(&self) -> kind_inference::Result<()> {
        for p in self.type_env.values().cloned() {
            let mut env = self.type_constructors.clone();
            env.extend(p.quantifiers.into_iter().map(|v| (v.to_name(), Kind::Var(v))));
            let mono = p.mono.substitute_constructors(&env);
            kind_inference::algorithm::m(&env, &mono, Kind::Type, 0)?;
        }
        Ok(())
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(self.main.clone().into(), &self.global, &self.built_ins)
    }

    pub fn types(&self) -> &Environment<PolyType> {
        &self.type_env
    }

    pub fn type_constructors(&self) -> &Environment<Kind> {
        &self.type_constructors
    }
}
