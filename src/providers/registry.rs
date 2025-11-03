use std::collections::HashMap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::Arc;

use crate::providers::TypeProvider;

#[derive(Clone, Default)]
pub struct Registry(HashMap<String, Arc<dyn TypeProvider>>);

impl Deref for Registry {
    type Target = HashMap<String, Arc<dyn TypeProvider>>;
    fn deref(&self) -> &HashMap<String, Arc<dyn TypeProvider>> {
        &self.0
    }
}

impl DerefMut for Registry {
    fn deref_mut(&mut self) -> &mut HashMap<String, Arc<dyn TypeProvider>> {
        &mut self.0
    }
}

impl Registry {
    pub fn register(&mut self, name: impl Into<String>, prov: Arc<dyn TypeProvider>) {
        self.insert(name.into(), prov);
    }
}
