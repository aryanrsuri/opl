use std::fmt;

#[derive(Debug)]
pub enum Object {
    Unit,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    List(Vec<Object>),

    // Return
    Return(Box<Object>),

    // Option
    OptionSome(Box<Object>),
    OptionNone,
    // Result
    ResultOk(Box<Object>),
    ResultErr(Box<Object>),

    // Type Errors
    TypeError(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Float(ref value) => write!(f, "{}", value),
            Object::Boolean(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "{}", value),
            Object::Unit => write!(f, "()"),
            Object::OptionSome(ref value) => write!(f, "Some({})", value),
            Object::OptionNone => write!(f, "None"),
            Object::List(ref value) => write!(f, "{:?}", value),
            Object::Return(ref value) => write!(f, "{}", value),
            Object::ResultOk(ref value) => write!(f, "{}", value),
            Object::ResultErr(ref value) => write!(f, "{}", value),
            Object::TypeError(ref value) => write!(f, "{}", value),
            // ref value => write!(f, "{}", value),
        }
    }
}
