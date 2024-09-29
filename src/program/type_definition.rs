use crate::kind_inference::{self, KindError};
use crate::model::typing::{Kind, Variable};
use crate::model::{DataConstructor, Environment, FreeVariable, Substitute, Substitution};
use std::{collections::HashSet, fmt::Display};
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct TypeDefinition {
    pub name: String,
    pub vars: Vec<Variable>,
    pub constructors: Vec<DataConstructor>,
}

#[derive(Clone, Error, Debug)]
pub enum TypeDefinitionError {
    #[error("unused type variable '{0}'")]
    UnusedTypeVar(Variable),
    #[error("unknown type variable '{0}'")]
    UnknownTypeVar(Variable),
    #[error("invalid data constructor '{0}'")]
    InvalidDataConstructor(DataConstructor, #[source] KindError),
}
impl Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = format!("type {}", self.name);
        if !self.vars.is_empty() {
            let vars = self.vars.iter().map(ToString::to_string).collect::<Vec<_>>();
            s = format!("{s} {}", vars.join(" "));
        }
        if !self.constructors.is_empty() {
            let sum = (self.constructors.iter())
                .map(ToString::to_string)
                .collect::<Vec<_>>();
            s = format!("{s} = {}", sum.join(" + "));
        }
        write!(f, "{s}")
    }
}

impl TypeDefinition {
    pub fn new(name: String, vars: Vec<Variable>, constructors: Vec<DataConstructor>) -> Self {
        Self {
            name,
            vars,
            constructors,
        }
    }

    pub fn infer_kind(&mut self, type_constructors: &Environment<Kind>) -> Result<Kind, TypeDefinitionError> {
        let used_vars = (self.constructors.iter())
            .flat_map(|con| con.params.iter())
            .fold(HashSet::new(), |vars, m| {
                vars.union(&m.free_vars()).copied().collect()
            });
        if let Some(&v) = HashSet::from_iter(&self.vars).difference(&used_vars).next() {
            return Err(TypeDefinitionError::UnusedTypeVar(v.clone()));
        };

        let kind = (self.vars.iter().rev()).fold(Kind::Type, |k, v| Kind::arrow(v.clone(), k));
        let mut env = type_constructors.clone() + (self.name.clone(), kind.clone());

        for con in &mut self.constructors {
            con.substitute_constructors_mut(&env);
        }

        env.extend(self.vars.iter().map(|v| (v.to_name(), Kind::Var(v.clone()))));
        let mut subst = Substitution::new();
        for con in &self.constructors {
            let p = con.get_type(&self.vars);
            if let Some(&v) = p.free_vars().iter().next() {
                return Err(TypeDefinitionError::UnknownTypeVar(v.clone()));
            }

            let mono = p.mono.substitute_constructors(&env);
            let (s, _) = kind_inference::algorithm::m(&env, &mono, Kind::Type, 0)
                .map_err(|e| TypeDefinitionError::InvalidDataConstructor(con.clone(), e))?;
            subst.combine_mut(&s);
        }

        let kind = kind.substitute(&subst);
        let subst = kind.vars().cloned().map(|v| (v, Kind::Type)).collect();
        Ok(kind.substitute(&subst))
    }
}
