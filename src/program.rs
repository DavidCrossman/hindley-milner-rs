use crate::environment::Environment;
use crate::expression::{Expression, Literal};
use crate::interpreter::{self, BuiltInFn, Value};
use crate::parser::{self, Item};
use crate::type_checking::model::{MonoType, PolyType};
use crate::type_checking::{self, substitution::Substitute};
use std::{fmt::Display, iter, sync::LazyLock};

#[derive(Clone)]
pub struct Program {
    main: Expression,
    definitions: Environment<Expression>,
    declarations: Environment<PolyType>,
    built_ins: Environment<BuiltInFn>,
}

#[derive(Clone, Debug)]
pub enum ProgramError {
    NoMain,
    MissingDefinition(String),
    UnknownBuiltIn(String),
    BuiltInMissingSignature(String),
    DuplicateDefinition(String),
    DuplicateSignature(String),
}

pub type Result<T> = std::result::Result<T, ProgramError>;

pub static BUILT_INS: LazyLock<Environment<BuiltInFn>> = LazyLock::new(|| {
    let mut env = Environment::new();
    env += (
        "add".to_owned(),
        std::sync::Arc::new(move |x| {
            let Value::Lit(Literal::Int(x)) = x else {
                unimplemented!()
            };
            Ok(Value::BuiltIn(std::sync::Arc::new(move |y| {
                let Value::Lit(Literal::Int(y)) = y else {
                    unimplemented!()
                };
                Ok(Value::Lit(Literal::Int(x + y)))
            })))
        }) as BuiltInFn,
    );
    env
});

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoMain => write!(f, "no 'main' definition found"),
            Self::MissingDefinition(s) => write!(f, "missing definition for '{s}'"),
            Self::UnknownBuiltIn(s) => write!(f, "unknown built-in function '{s}'"),
            Self::BuiltInMissingSignature(s) => {
                write!(f, "missing type signature for built-in function '{s}'")
            }
            Self::DuplicateDefinition(s) => write!(f, "duplicate definitions for '{s}'"),
            Self::DuplicateSignature(s) => write!(f, "duplicate type signatures for '{s}'"),
        }
    }
}

impl Program {
    pub fn new(items: impl IntoIterator<Item = parser::Item>) -> Result<Self> {
        let mut main = None;
        let mut definitions = Environment::new();
        let mut declarations = Environment::new();
        let mut built_ins = Environment::new();
        for item in items {
            match item {
                Item::Definition(name, expr) => {
                    if definitions.contains_name(&name) || built_ins.contains_name(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    } else if name == "main" {
                        main = Some(expr);
                    } else {
                        definitions += (name, expr);
                    }
                }
                Item::Declaration(name, m) => {
                    if declarations.contains_name(&name) {
                        return Err(ProgramError::DuplicateSignature(name));
                    }
                    declarations += (name, m.generalise(&declarations));
                }
                Item::BuiltInDefinition(name) => {
                    if definitions.contains_name(&name) || built_ins.contains_name(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    }
                    let Some(fun) = BUILT_INS.get(&name) else {
                        return Err(ProgramError::UnknownBuiltIn(name));
                    };
                    built_ins += (name, fun.clone());
                }
            }
        }
        if let Some(name) = (declarations.names())
            .find(|name| !definitions.contains_name(name) && !built_ins.contains_name(name))
        {
            return Err(ProgramError::MissingDefinition(name.clone()));
        }
        if let Some(name) = built_ins.names().find(|name| !declarations.contains_name(name)) {
            return Err(ProgramError::BuiltInMissingSignature(name.clone()));
        }
        Ok(Self {
            main: main.ok_or(ProgramError::NoMain)?,
            definitions,
            declarations,
            built_ins,
        })
    }

    pub fn type_check(&self) -> type_checking::Result<Environment<PolyType>> {
        let mut env = self.declarations.clone();
        let main_def = (&"main".to_owned(), &self.main);
        for (name, expr) in self.definitions.iter().chain(iter::once(main_def)) {
            let (t, n) = match env.remove(name) {
                Some(p) => p.instantiate(0),
                None => (MonoType::Var(0.into()), 1),
            };
            let (s, _) = type_checking::algorithm::m(&env, expr, t.clone(), n)?;
            env += (name.clone(), t.substitute(&s).generalise(&env));
        }
        Ok(env)
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(&self.main, &self.definitions, &self.built_ins)
    }
}
