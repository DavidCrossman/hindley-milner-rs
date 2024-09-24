mod built_in;
mod expression;
mod item;
mod type_definition;
mod value;

pub use built_in::BuiltInFn;
pub use expression::Expression;
pub use item::Item;
pub use type_definition::{DataConstructor, TypeDefinition, TypeDefinitionError};
pub use value::{Value, ValueConversionError};

use crate::kind_inference::{self, KindError};
use crate::model::term::Term;
use crate::model::typing::{Kind, PolyType};
use crate::model::Environment;
use crate::type_inference::TypeError;
use crate::{interpreter, type_inference};
use std::collections::HashSet;
use thiserror::Error;

#[derive(Clone)]
pub struct Program {
    main: Term,
    global: Environment<Expression>,
    types: Environment<PolyType>,
    type_constructors: Environment<Kind>,
    built_ins: Vec<BuiltInFn>,
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
    #[error("type definition error")]
    TypeDefinitionError(#[source] TypeDefinitionError),
    #[error("type error in term: {0}")]
    TypeError(Term, #[source] TypeError),
    #[error("kind error in type: {0}")]
    KindError(PolyType, #[source] KindError),
}

pub type Result<T> = std::result::Result<T, ProgramError>;

impl Program {
    pub fn new(items: impl IntoIterator<Item = item::Item>) -> Result<Self> {
        let mut main = None;
        let mut term_definitions = Vec::new();
        let mut data_constructors = Vec::new();
        let mut built_ins = Vec::<BuiltInFn>::new();
        let mut types = Environment::new();
        let mut type_constructors = Environment::new();
        let mut used_def_names = HashSet::new();
        type_constructors += ("Unit".to_owned(), Kind::Type);
        type_constructors += ("Int".to_owned(), Kind::Type);
        for item in items {
            match item {
                Item::TermDefinition(name, term) => {
                    if used_def_names.contains(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    } else if name == "main" {
                        used_def_names.insert(name);
                        main = Some(term);
                    } else {
                        used_def_names.insert(name.clone());
                        term_definitions.push((name, term));
                    }
                }
                Item::BuiltInDefinition(name) => {
                    if used_def_names.contains(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    }
                    let Some(fun) = built_in::BUILT_INS.get(&name).cloned() else {
                        return Err(ProgramError::UnknownBuiltIn(name));
                    };
                    used_def_names.insert(name);
                    built_ins.push(fun);
                }
                Item::TypeDefinition(type_def) => {
                    let (constructor_definitions, constructor_types) = type_def
                        .to_constructors(&mut type_constructors)
                        .map_err(ProgramError::TypeDefinitionError)?;

                    if let Some(name) = constructor_types.names().find(|&name| types.contains_name(name)) {
                        return Err(ProgramError::DuplicateSignature(name.clone()));
                    }
                    if let Some(name) = constructor_definitions
                        .names()
                        .find(|&name| used_def_names.contains(name))
                    {
                        return Err(ProgramError::DuplicateDefinition(name.clone()));
                    }

                    used_def_names.extend(constructor_definitions.names().cloned());
                    data_constructors.extend(constructor_definitions);
                    types.extend(constructor_types);
                }
                Item::TypeDeclaration(name, m) => {
                    if types.contains_name(&name) {
                        return Err(ProgramError::DuplicateSignature(name));
                    }
                    let p = m.substitute_constructors(&type_constructors).generalise(&types);
                    types += (name, p);
                }
            }
        }
        if let Some(name) = types.names().find(|&name| !used_def_names.contains(name)) {
            return Err(ProgramError::MissingDefinition(name.clone()));
        }
        if let Some(f) = built_ins.iter().find(|f| !types.contains_name(f.name())) {
            return Err(ProgramError::BuiltInMissingSignature(f.name().to_owned()));
        }

        let main = main.ok_or(ProgramError::NoMain)?;

        term_definitions
            .iter()
            .map(|(n, t)| (n.as_str(), t))
            .chain(std::iter::once(("main", &main)))
            .try_for_each(|(name, term)| {
                type_inference::infer_type(name, term, &types)
                    .map(|p| types += (name.to_owned(), p))
                    .map_err(|e| ProgramError::TypeError(term.clone(), e))
            })?;

        types.values().try_for_each(|p| {
            kind_inference::kind_check(p, Kind::Type, &type_constructors)
                .map_err(|e| ProgramError::KindError(p.clone(), e))
        })?;

        Ok(Self {
            main,
            global: term_definitions
                .into_iter()
                .map(|(name, term)| (name, Expression::Term(term)))
                .chain(data_constructors)
                .collect(),
            types,
            type_constructors,
            built_ins,
        })
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(self.main.clone().into(), &self.global, &self.built_ins)
    }

    pub fn types(&self) -> &Environment<PolyType> {
        &self.types
    }

    pub fn type_constructors(&self) -> &Environment<Kind> {
        &self.type_constructors
    }
}
