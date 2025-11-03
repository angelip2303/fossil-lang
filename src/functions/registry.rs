use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;

use super::Function;

#[derive(Clone, Default)]
pub struct Registry(HashMap<String, Arc<dyn Function>>);

impl Deref for Registry {
    type Target = HashMap<String, Arc<dyn Function>>;
    fn deref(&self) -> &HashMap<String, Arc<dyn Function>> {
        &self.0
    }
}

impl DerefMut for Registry {
    fn deref_mut(&mut self) -> &mut HashMap<String, Arc<dyn Function>> {
        &mut self.0
    }
}

impl Registry {
    pub fn register(&mut self, name: impl Into<String>, func: Arc<dyn Function>) {
        self.insert(name.into(), func);
    }
}
