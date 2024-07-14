use crate::{expression::Object, interpreter::{Interpreter, Value}};

pub(crate) trait Callable {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Value, String>;
    fn arity(&self, interpreter: &mut Interpreter) -> Result<u8, String>;
}
