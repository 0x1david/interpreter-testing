use std::collections::HashMap;

use crate::interpreter::Value;

pub(crate) struct Environment {
    map: HashMap<String, Value>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            map: HashMap::new()
        }
    }
    pub fn define(&mut self, name: String, val: Value) {
        self.map.insert(name.to_string(), val);
    }
    pub fn read(&self, name: &str) -> Result<Value, String>{
        self.map.get(name).cloned().ok_or("Undefined variable name.".to_string())
    }
}
