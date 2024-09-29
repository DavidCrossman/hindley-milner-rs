use super::typing::{Kind, MonoType, PolyType, Variable};
use super::Environment;
use crate::program::{BuiltInFn, Value};
use std::fmt::Display;

#[derive(Clone, Debug)]
pub struct DataConstructor {
    pub name: String,
    pub return_type: MonoType,
    pub params: Vec<MonoType>,
}

impl Display for DataConstructor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.params.is_empty() {
            write!(f, "{}", self.name)
        } else {
            let params = (self.params.iter())
                .map(|m| match m {
                    MonoType::App(..) => format!("({m})"),
                    MonoType::Var(_) | MonoType::Arrow | MonoType::Con(_) => m.to_string(),
                })
                .collect::<Vec<_>>();
            write!(f, "{} {}", self.name, params.join(" "))
        }
    }
}

impl DataConstructor {
    pub fn new(name: String, return_type: MonoType, params: Vec<MonoType>) -> Self {
        Self {
            name,
            return_type,
            params,
        }
    }

    pub fn substitute_constructors(mut self, type_constructors: &Environment<Kind>) -> Self {
        self.substitute_constructors_mut(type_constructors);
        self
    }

    pub fn substitute_constructors_mut(&mut self, type_constructors: &Environment<Kind>) {
        self.params
            .iter_mut()
            .for_each(|mono| mono.substitute_constructors_mut(type_constructors));
    }

    pub fn get_value(&self) -> Value {
        if self.params.is_empty() {
            Value::Data(self.name.clone(), Vec::new())
        } else {
            BuiltInFn::make_data_constructor(self.name.clone(), self.params.len()).into()
        }
    }

    pub fn get_type(&self, vars: &[Variable]) -> PolyType {
        let mono = if self.params.is_empty() {
            self.return_type.clone()
        } else {
            (self.params.iter().cloned().rev())
                .fold(self.return_type.clone(), |m1, m2| MonoType::function(m2, m1))
        };
        PolyType::new(mono, vars.to_owned())
    }
}
