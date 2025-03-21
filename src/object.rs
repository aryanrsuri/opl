use crate::ast::{Identifier, Statement};
use crate::environment::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Unit,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    List(Vec<Object>),

    Function(Vec<Identifier>, Vec<Statement>, Rc<RefCell<Env>>),

    Return(Box<Object>),

    // Option
    OptionSome(Box<Object>),
    OptionNone,
    // Result
    ResultOk(Box<Object>),
    ResultErr(Box<Object>),

    // Type Errors
    Error(String),

    // Builtin
    Builtin(fn(Vec<Object>) -> Object),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Float(ref value) => write!(f, "{}", value),
            Object::Boolean(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "\"{}\"", value),
            Object::Unit => write!(f, "()"),
            Object::OptionSome(ref value) => write!(f, "Some({})", value),
            Object::OptionNone => write!(f, "None"),
            Object::Function(ref parameters, _, _) => {
                write!(f, "fn {:?} -> {{ ... }}", parameters)
            }
            Object::List(ref value) => write!(f, "[{}]", value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
            Object::Return(ref value) => write!(f, "{}", value),
            Object::ResultOk(ref value) => write!(f, "{}", value),
            Object::ResultErr(ref value) => write!(f, "{}", value),
            Object::Error(ref value) => write!(f, "{}", value),
            Object::Builtin(ref value) => write!(f, "{:?}", value),
        }
    }
}
