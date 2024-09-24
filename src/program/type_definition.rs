use crate::kind_inference::{self, KindError};
use crate::model::typing::{Kind, MonoType, PolyType, Variable};
use crate::model::{Environment, FreeVariable, Substitute, Substitution};
use crate::program::{BuiltInFn, Expression, Value};
use std::collections::HashSet;
use std::fmt::Display;
use thiserror::Error;

#[derive(Clone, Debug)]
pub struct DataConstructor {
    pub name: String,
    pub types: Vec<MonoType>,
}

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
    #[error("invalid type constructor '{0}'")]
    InvalidTypeConstructor(String, #[source] KindError),
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
    pub fn new(
        name: impl AsRef<str>,
        vars: impl IntoIterator<Item = Variable>,
        constructors: impl IntoIterator<Item = DataConstructor>,
    ) -> Self {
        Self {
            name: name.as_ref().to_owned(),
            vars: vars.into_iter().collect(),
            constructors: constructors.into_iter().collect(),
        }
    }

    pub fn to_constructors(
        self,
        type_constructors: &mut Environment<Kind>,
    ) -> Result<(Environment<Expression>, Environment<PolyType>), TypeDefinitionError> {
        let used_vars = (self.constructors.iter())
            .flat_map(|con| con.types.iter())
            .fold(HashSet::new(), |vars, m| {
                vars.union(&m.free_vars()).copied().collect()
            });
        if let Some(&v) = HashSet::from_iter(&self.vars).difference(&used_vars).next() {
            return Err(TypeDefinitionError::UnusedTypeVar(v.clone()));
        };

        let kind = (self.vars.iter().rev()).fold(Kind::Type, |k, v| Kind::arrow(v.clone(), k));
        *type_constructors += (self.name.clone(), kind.clone());
        let mono = self.vars.iter().fold(MonoType::Con(self.name.clone()), |m, v| {
            MonoType::App(Box::new(m), Box::new(MonoType::Var(v.clone())))
        });

        let mut constructor_definitions = Environment::new();
        let mut constructor_types = Environment::new();
        let mut subst = Substitution::new();
        for DataConstructor { name, types } in self.constructors {
            let mut mono = mono.clone();
            let expr = if types.is_empty() {
                Expression::Value(Value::Data(name.clone(), Vec::new()))
            } else {
                let fun = BuiltInFn::make_data_constructor(name.clone(), types.len());
                mono = (types.into_iter().rev()).fold(mono, |m1, m2| MonoType::function(m2, m1));
                mono.substitute_constructors_mut(type_constructors);
                Expression::Value(Value::BuiltIn(fun))
            };

            let p = PolyType::new(mono.clone(), self.vars.clone());
            if let Some(&v) = p.free_vars().iter().next() {
                return Err(TypeDefinitionError::UnknownTypeVar(v.clone()));
            }
            constructor_definitions += (name.clone(), expr);
            constructor_types += (name.clone(), p);

            let mut env = type_constructors.clone();
            env.extend(self.vars.iter().map(|v| (v.to_name(), Kind::Var(v.clone()))));
            mono.substitute_constructors_mut(&env);
            let (s, _) = kind_inference::algorithm::m(&env, &mono, Kind::Type, 0)
                .map_err(|e| TypeDefinitionError::InvalidTypeConstructor(name, e))?;
            subst.combine_mut(&s);
        }

        let kind = kind.substitute(&subst);
        let subst = kind.vars().cloned().map(|v| (v, Kind::Type)).collect();
        *type_constructors += (self.name, kind.substitute(&subst));

        Ok((constructor_definitions, constructor_types))
    }
}
