use crate::ast::{Identifier, Statement, Namespace};
use crate::environment::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

#[derive(PartialEq, Debug, Clone)]
pub enum Object {
    Unit,
    Integer(i128),
    Float(f64),
    Boolean(bool),
    String(String),
    List(Vec<Object>),
    Record {
        type_name: String,
        fields: Vec<(Identifier, Object)>,
    },
    Tuple(Vec<Object>),
    TypeDefinition {
        name: String,
        fields: Vec<(String, String)>, // field_name -> type_name
    },
    Function(Vec<Identifier>, Vec<Statement>, Rc<RefCell<Env>>),
    Return(Box<Object>),
    OptionSome(Box<Object>),
    OptionNone,
    ResultOk(Box<Object>),
    ResultErr(Box<Object>),
    Error(String),
    Builtin(fn(Vec<Object>) -> Object),
    BuiltinMethod {
        namespace: Namespace,
        method: String,
        receiver: Box<Object>,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Float(ref value) => write!(f, "{}", value),
            Object::Boolean(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "\"{}\"", value),
            Object::Unit => write!(f, "()"),
            Object::OptionSome(ref value) => write!(f, "Some {}", value),
            Object::OptionNone => write!(f, "None"),
            Object::Function(ref parameters, _, _) => {
                write!(f, "fn {} -> {{ ... }}", parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "))
            }
            Object::List(ref value) => write!(f, "[{}]", value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
            Object::Record { type_name, fields } => {
                write!(f, "{} {{ {} }}", type_name, fields.iter()
                    .map(|(k, v)| format!("{} = {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", "))
            },
            Object::TypeDefinition { name, fields } => {
                write!(f, "type {} = {{ {} }}", name, fields.iter()
                    .map(|(name, type_name)| format!("{}: {}", name, type_name))
                    .collect::<Vec<String>>()
                    .join(", "))
            },
            Object::Return(ref value) => write!(f, "{}", value),
            Object::ResultOk(ref value) => write!(f, "Ok {}", value),
            Object::ResultErr(ref value) => write!(f, "Err {}", value),
            Object::Error(ref value) => write!(f, "{}", value),
            Object::Builtin(ref value) => write!(f, "{:?}", value),
            Object::BuiltinMethod { namespace, method, .. } => write!(f, "{:?}.{}", namespace, method),
            Object::Tuple(ref value) => write!(f, "({})", value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
        }
    }
}
