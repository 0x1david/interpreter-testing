use std::collections::HashMap;

use crate::interpreter::Value;

/// A struct representing an environment for storing variable names and values.
pub(crate) struct Environment {
    map: HashMap<String, Value>,
}

impl Environment {
    /// Creates a new, empty Environment.
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    /// Defines a new variable in the environment with the given name and value.
    ///
    /// # Arguments
    ///
    /// * `name` - A String representing the name of the variable.
    /// * `val` - The value to associate with the variable.
    pub fn define(&mut self, name: String, val: Value) {
        self.map.insert(name.to_string(), val);
    }

    /// Reads the value associated with the given variable name from the environment.
    ///
    /// # Arguments
    ///
    /// * `name` - A string slice representing the name of the variable to read.
    ///
    /// # Returns
    ///
    /// * `Ok(Value)` - If the variable exists in the environment.
    /// * `Err(String)` - If the variable is not defined in the environment.
    pub fn read(&self, name: &str) -> Result<Value, String> {
        self.map
            .get(name)
            .cloned()
            .ok_or("Undefined variable name.".to_string())
    }
}
