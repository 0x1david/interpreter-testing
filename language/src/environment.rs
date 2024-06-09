use std::{borrow::Borrow, cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use crate::interpreter::Value;

/// A struct representing an environment for storing variable names and values.
#[derive(Clone)]
pub(crate) struct Environment {
    pub outer: Option<Rc<RefCell<Environment>>>,
    map: HashMap<String, Value>,
}

impl Environment {
    /// Creates a new, empty Environment.
    pub fn new() -> Self {
        Self {
            outer: None,
            map: HashMap::new(),
        }
    }

    pub fn new_scoped(outer: Rc<RefCell<Environment>>) -> Self {
        Self {
            outer: Some(outer),
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

    /// Reads the value associated with the given variable name from the local environment or any
    /// of the outer ones.
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
            .or_else(|| match &self.outer {
                Some(outer) => outer.deref().borrow().read(name).ok(),
                None => None,
            })
            .ok_or("Variable not defined".to_string())
    }
}
