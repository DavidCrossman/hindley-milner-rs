mod built_in;
mod item;

pub use built_in::BuiltInFn;
pub use item::{DataConstructor, Item};

use crate::interpreter::{self, Control, Value};
use crate::model::expression::Expression;
use crate::model::typing::{Kind, MonoType, PolyType, Variable};
use crate::model::{Environment, FreeVariable, Substitute};
use crate::type_inference;
use std::collections::HashSet;
use thiserror::Error;

#[derive(Clone)]
pub struct Program {
    main: Expression,
    global: Environment<Control>,
    declarations: Environment<PolyType>,
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
}

pub type Result<T> = std::result::Result<T, ProgramError>;

impl Program {
    pub fn new(items: impl IntoIterator<Item = item::Item>) -> Result<Self> {
        let mut main = None;
        let mut global = Environment::new();
        let mut declarations = Environment::new();
        let mut type_constructors = Environment::new();
        let mut built_ins = Environment::new();
        type_constructors += ("Unit".to_owned(), Kind::Type);
        type_constructors += ("Int".to_owned(), Kind::Type);
        for item in items {
            match item {
                Item::TermDefinition(name, expr) => {
                    if global.contains_name(&name)
                        || built_ins.contains_name(&name)
                        || main.is_some() && name == "main"
                    {
                        return Err(ProgramError::DuplicateDefinition(name));
                    } else if name == "main" {
                        main = Some(expr);
                    } else {
                        global += (name, Control::Expr(expr));
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
                    let kind = (0..vars.len())
                        .fold(Kind::Type, |k, _| Kind::Arrow(Box::new(Kind::Type), Box::new(k)));
                    type_constructors += (type_name.clone(), kind);
                    let mono = (vars.iter()).fold(MonoType::Con(type_name.clone().into()), |m, s| {
                        MonoType::App(Box::new(m), Box::new(MonoType::Var(s.clone())))
                    });
                    for DataConstructor { name, types } in sum {
                        let mut mono = mono.clone();
                        let c;
                        if types.is_empty() {
                            c = Control::Val(Value::Data(name.clone(), Vec::new()));
                        } else {
                            let arity = types.len();
                            mono = (types.into_iter().rev()).fold(mono, |m1, m2| MonoType::function(m2, m1));
                            mono.traverse(&mut |m| {
                                if let MonoType::Var(Variable::Named(name)) = &m {
                                    if type_constructors.contains_name(name) {
                                        *m = MonoType::Con(name.clone().into());
                                    }
                                }
                            });
                            let fun = BuiltInFn::make_data_constructor(name.clone(), arity);
                            c = Control::Val(Value::BuiltIn(fun));
                        }
                        let p = PolyType::new(mono, vars.clone());
                        if let Some(&t) = p.free_vars().iter().next() {
                            return Err(ProgramError::UnknownTypeVar(t.clone()));
                        }
                        if declarations.contains_name(&name) {
                            return Err(ProgramError::DuplicateSignature(name));
                        }
                        if global.contains_name(&name) || built_ins.contains_name(&name) || name == "main" {
                            return Err(ProgramError::DuplicateDefinition(name));
                        }
                        global += (name.clone(), c);
                        declarations += (name, p);
                    }
                }
                Item::Declaration(name, mut m) => {
                    if declarations.contains_name(&name) {
                        return Err(ProgramError::DuplicateSignature(name));
                    }
                    m.traverse(&mut |m| {
                        if let MonoType::Var(Variable::Named(name)) = &m {
                            if type_constructors.contains_name(name) {
                                *m = MonoType::Con(name.clone().into());
                            }
                        }
                    });
                    declarations += (name, m.generalise(&declarations));
                }
            }
        }
        if let Some(name) = (declarations.names())
            .find(|name| !global.contains_name(name) && !built_ins.contains_name(name) && name != &"main")
        {
            return Err(ProgramError::MissingDefinition(name.clone()));
        }
        if let Some(name) = built_ins.names().find(|name| !declarations.contains_name(name)) {
            return Err(ProgramError::BuiltInMissingSignature(name.clone()));
        }
        Ok(Self {
            main: main.ok_or(ProgramError::NoMain)?,
            global,
            declarations,
            type_constructors,
            built_ins,
        })
    }

    pub fn type_check(&self) -> type_inference::Result<Environment<PolyType>> {
        (self.global.iter())
            .filter_map(|(name, c)| match c {
                Control::Expr(e) => Some((name, e)),
                _ => None,
            })
            .chain(std::iter::once((&"main".to_owned(), &self.main)))
            .try_fold(self.declarations.clone(), |mut env, (name, expr)| {
                let (t, n) = match env.remove(name) {
                    Some(p) => p.instantiate(0),
                    None => (MonoType::Var(0.into()), 1),
                };
                let (s, _) = type_inference::algorithm::m(&env, expr, t.clone(), n)?;
                let p = t.substitute(&s).generalise(&env);
                Ok(env + (name.clone(), p))
            })
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(&self.main, &self.global, &self.built_ins)
    }
}
