use crate::built_in::{self, BuiltInFn};
use crate::environment::Environment;
use crate::expression::Expression;
use crate::interpreter::{self, Control, Value};
use crate::parser::{self, DataConstructor, Item};
use crate::type_checking::model::{MonoType, PolyType, TypeConstructor};
use crate::type_checking::{self, substitution::Substitute};
use thiserror::Error;

#[derive(Clone)]
pub struct Program {
    main: Expression,
    global: Environment<Control>,
    declarations: Environment<PolyType>,
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
}

pub type Result<T> = std::result::Result<T, ProgramError>;

impl Program {
    pub fn new(items: impl IntoIterator<Item = parser::Item>) -> Result<Self> {
        let mut main = None;
        let mut global = Environment::new();
        let mut declarations = Environment::new();
        let mut built_ins = Environment::new();
        for item in items {
            match item {
                Item::ValueDefinition(name, expr) => {
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
                Item::TypeDefinition(type_name, sum) => sum.into_iter().for_each(|cons| {
                    let DataConstructor { name, params } = cons;
                    if params.is_empty() {
                        let m = MonoType::Con(TypeConstructor::Custom(type_name.clone()));
                        declarations += (name.clone(), m.generalise(&declarations));
                        global += (name.clone(), Control::Val(Value::Custom(name, Vec::new())))
                    } else {
                        let arity = params.len();
                        let m = params.into_iter().rev().fold(
                            MonoType::Con(TypeConstructor::Custom(type_name.clone())),
                            |m, con| TypeConstructor::Function(Box::new(con.into()), Box::new(m)).into(),
                        );
                        declarations += (name.clone(), m.generalise(&declarations));
                        global += (
                            name.clone(),
                            Control::Val(Value::BuiltIn(BuiltInFn::make_data_constructor(name, arity))),
                        );
                    }
                }),
                Item::Declaration(name, m) => {
                    if declarations.contains_name(&name) {
                        return Err(ProgramError::DuplicateSignature(name));
                    }
                    declarations += (name, m.generalise(&declarations));
                }
            }
        }
        if let Some(name) =
            (declarations.names()).find(|name| !global.contains_name(name) && !built_ins.contains_name(name))
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
            built_ins,
        })
    }

    pub fn type_check(&self) -> type_checking::Result<Environment<PolyType>> {
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
                let (s, _) = type_checking::algorithm::m(&env, expr, t.clone(), n)?;
                let p = t.substitute(&s).generalise(&env).requantify();
                Ok(env + (name.clone(), p))
            })
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(&self.main, &self.global, &self.built_ins)
    }
}
