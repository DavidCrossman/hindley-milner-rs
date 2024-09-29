mod built_in;
mod expression;
mod item;
mod type_definition;
mod value;

pub use built_in::BuiltInFn;
pub use expression::Expression;
pub use item::Item;
pub use type_definition::{TypeDefinition, TypeDefinitionError};
pub use value::{Value, ValueConversionError};

use crate::kind_inference::{self, KindError};
use crate::model::term::Term;
use crate::model::typing::{Kind, PolyType};
use crate::model::{DataConstructor, Environment};
use crate::type_inference::TypeError;
use crate::{interpreter, type_inference};
use std::collections::{HashMap, HashSet};
use thiserror::Error;

#[derive(Clone)]
pub struct Program {
    main: Term,
    global: Environment<Expression>,
    types: Environment<PolyType>,
    data_constructors: Environment<DataConstructor>,
    type_constructors: Environment<Kind>,
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
        let mut value_definitions = HashMap::new();
        let mut types = Environment::new();
        let mut data_constructors = Environment::new();
        let mut type_constructors = Environment::new();
        let mut used_expr_def_names = HashSet::new();
        type_constructors += ("Unit".to_owned(), Kind::Type);
        type_constructors += ("Int".to_owned(), Kind::Type);
        for item in items {
            match item {
                Item::TermDefinition(name, term) => {
                    if used_expr_def_names.contains(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    } else if name == "main" {
                        used_expr_def_names.insert(name);
                        main = Some(term);
                    } else {
                        used_expr_def_names.insert(name.clone());
                        term_definitions.push((name, term));
                    }
                }
                Item::BuiltInDefinition(name) => {
                    if used_expr_def_names.contains(&name) {
                        return Err(ProgramError::DuplicateDefinition(name));
                    }
                    let Some(fun) = built_in::BUILT_INS.get(&name).cloned() else {
                        return Err(ProgramError::UnknownBuiltIn(name));
                    };
                    used_expr_def_names.insert(name.clone());
                    value_definitions.insert(name, fun.into());
                }
                Item::TypeDefinition(mut type_def) => {
                    if type_constructors.contains_name(&type_def.name) {
                        return Err(ProgramError::DuplicateDefinition(type_def.name));
                    }

                    let kind = type_def
                        .infer_kind(&type_constructors)
                        .map_err(ProgramError::TypeDefinitionError)?;
                    type_constructors += (type_def.name, kind);

                    for con in type_def.constructors {
                        if types.contains_name(&con.name) {
                            return Err(ProgramError::DuplicateSignature(con.name));
                        }
                        if used_expr_def_names.contains(&con.name) {
                            return Err(ProgramError::DuplicateDefinition(con.name));
                        }

                        used_expr_def_names.insert(con.name.clone());
                        value_definitions.insert(con.name.clone(), con.get_value());
                        types += (con.name.clone(), con.get_type(&type_def.vars));
                        data_constructors += (con.name.clone(), con);
                    }
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
        if let Some(name) = types.names().find(|&name| !used_expr_def_names.contains(name)) {
            return Err(ProgramError::MissingDefinition(name.clone()));
        }
        if let Some(name) = value_definitions.keys().find(|name| !types.contains_name(name)) {
            return Err(ProgramError::BuiltInMissingSignature(name.clone()));
        }

        let main = main.ok_or(ProgramError::NoMain)?;

        term_definitions
            .iter()
            .map(|(n, t)| (n.as_str(), t))
            .chain(std::iter::once(("main", &main)))
            .try_for_each(|(name, term)| {
                type_inference::infer_type(name, term, &types, &data_constructors)
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
                .chain(
                    value_definitions
                        .into_iter()
                        .map(|(name, value)| (name, Expression::Value(value))),
                )
                .collect(),
            types,
            data_constructors,
            type_constructors,
        })
    }

    pub fn run(&self) -> interpreter::Result<Value> {
        interpreter::eval(self.main.clone().into(), &self.global)
    }

    pub fn types(&self) -> &Environment<PolyType> {
        &self.types
    }

    pub fn data_constructors(&self) -> &Environment<DataConstructor> {
        &self.data_constructors
    }

    pub fn type_constructors(&self) -> &Environment<Kind> {
        &self.type_constructors
    }
}
