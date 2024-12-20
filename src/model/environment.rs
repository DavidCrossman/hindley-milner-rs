use std::collections::{hash_map, HashMap};
use std::fmt::Display;
use std::ops::{Add, AddAssign};

#[derive(Default, PartialEq, Eq, Clone, Debug)]
pub struct Environment<T> {
    map: HashMap<String, T>,
}

impl<T> IntoIterator for Environment<T> {
    type Item = (String, T);
    type IntoIter = hash_map::IntoIter<String, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Environment<T> {
    type Item = (&'a String, &'a T);
    type IntoIter = hash_map::Iter<'a, String, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<T> FromIterator<(String, T)> for Environment<T> {
    fn from_iter<I: IntoIterator<Item = (String, T)>>(iter: I) -> Self {
        Self {
            map: HashMap::from_iter(iter),
        }
    }
}

impl<T> Extend<(String, T)> for Environment<T> {
    fn extend<I: IntoIterator<Item = (String, T)>>(&mut self, iter: I) {
        self.map.extend(iter)
    }
}

impl<T> AddAssign<(String, T)> for Environment<T> {
    fn add_assign(&mut self, (name, x): (String, T)) {
        self.map.insert(name, x);
    }
}

impl<T> Add<(String, T)> for Environment<T> {
    type Output = Self;

    fn add(mut self, rhs: (String, T)) -> Self::Output {
        self += rhs;
        self
    }
}

impl<T: Display> Display for Environment<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let assignments = self.iter().map(|(n, x)| format!("{n}={x}")).collect::<Vec<_>>();
        write!(f, "[{}]", assignments.join(","))
    }
}

impl<T> Environment<T> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn contains_name(&self, name: &str) -> bool {
        self.map.contains_key(name)
    }

    pub fn get(&self, name: &str) -> Option<&T> {
        self.map.get(name)
    }

    pub fn remove(&mut self, name: &str) -> Option<T> {
        self.map.remove(name)
    }

    pub fn iter(&self) -> hash_map::Iter<'_, String, T> {
        self.map.iter()
    }

    pub fn names(&self) -> hash_map::Keys<'_, String, T> {
        self.map.keys()
    }

    pub fn values(&self) -> hash_map::Values<'_, String, T> {
        self.map.values()
    }

    pub fn values_mut(&mut self) -> hash_map::ValuesMut<'_, String, T> {
        self.map.values_mut()
    }
}
