use super::type_definition::TypeDefinition;
use crate::model::term::Term;
use crate::model::typing::MonoType;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Item {
    TypeDefinition(TypeDefinition),
    TermDefinition(String, Term),
    BuiltInDefinition(String, MonoType),
    TypeDeclaration(String, MonoType),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeDefinition(type_def) => type_def.fmt(f),
            Self::TermDefinition(name, term) => write!(f, "{name} = {term}"),
            Self::BuiltInDefinition(name, mono) => write!(f, "builtin {name} : {mono}"),
            Self::TypeDeclaration(name, mono) => write!(f, "{name} : {mono}"),
        }
    }
}
