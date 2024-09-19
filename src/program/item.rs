use crate::model::term::Term;
use crate::model::typing::{MonoType, Variable};
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct DataConstructor {
    pub name: String,
    pub types: Vec<MonoType>,
}

#[derive(Clone, Debug)]
pub enum Item {
    TypeDefinition(String, Vec<Variable>, Vec<DataConstructor>),
    TermDefinition(String, Term),
    BuiltInDefinition(String),
    Declaration(String, MonoType),
}

impl Display for DataConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.types.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let types = (self.types.iter())
                .map(|m| match m {
                    MonoType::App(..) => format!("({m})"),
                    MonoType::Var(_) | MonoType::Arrow | MonoType::Con(_) => m.to_string(),
                })
                .collect::<Vec<_>>();
            write!(f, "{} {}", self.name, types.join(" "))
        }
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::TypeDefinition(s, params, cons) => {
                let mut s = format!("type {s}");
                if !params.is_empty() {
                    let params = params.iter().map(ToString::to_string).collect::<Vec<_>>();
                    s = format!("{s} {}", params.join(" "));
                }
                if !cons.is_empty() {
                    let sum = cons.iter().map(ToString::to_string).collect::<Vec<_>>();
                    s = format!("{s} = {}", sum.join(" + "));
                }
                write!(f, "{s}")
            }
            Self::TermDefinition(s, t) => write!(f, "{s} = {t}"),
            Self::BuiltInDefinition(s) => write!(f, "{s} = builtin"),
            Self::Declaration(s, p) => write!(f, "{s} : {p}"),
        }
    }
}
