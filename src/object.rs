use crate::ast::{Identifier, Statement, Namespace};
use crate::environment::Env;
use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;
use std::collections::HashMap;
use crate::ast::Type;

#[derive(PartialEq, Debug, Clone)]
pub enum TypeDefinitionInner {
    Record(Vec<(String, Type)>),             // field_name -> type
    Union(Vec<(String, Option<Type>)>),      // variant_name -> optional associated type
    Alias(Type),                             // type alias
}

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
    Union {
        type_name: String,
        variant: String,
        value: Option<Box<Object>>,          // Optional value for variant
    },
    Tuple(Vec<Object>),
    TypeDefinition {
        name: String,
        inner: TypeDefinitionInner,
    },
    Function(Vec<Identifier>, Vec<Statement>, Rc<RefCell<Env>>, Rc<RefCell<HashMap<String, Type>>>),
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
            Object::Function(ref parameters, _, _, _) => {
                write!(f, "fn {} -> {{ ... }}", parameters.iter().map(|p| p.to_string()).collect::<Vec<String>>().join(", "))
            }
            Object::List(ref value) => write!(f, "[{}]", value.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
            Object::Record { type_name, fields } => {
                write!(f, "{} {{ {} }}", type_name, fields.iter()
                    .map(|(k, v)| format!("{} = {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", "))
            },
            Object::TypeDefinition { name, inner } => {
                match inner {
                    TypeDefinitionInner::Record(fields) => {
                        if fields.is_empty() {
                            write!(f, "type {} = {{}}", name)
                        } else {
                            write!(f, "type {} = {{ {} }}", name, fields.iter()
                                .map(|(field_name, field_type)| format!("{}: {:?}", field_name, field_type))
                                .collect::<Vec<String>>()
                                .join(", "))
                        }
                    },
                    TypeDefinitionInner::Union(variants) => {
                        write!(f, "type {} = {}", name, variants.iter()
                            .map(|(variant_name, opt_type)| {
                                match opt_type {
                                    Some(t) => format!("{} of {:?}", variant_name, t),
                                    None => format!("{}", variant_name)
                                }
                            })
                            .collect::<Vec<_>>()
                            .join(" | "))
                    },
                    TypeDefinitionInner::Alias(typ) => {
                        write!(f, "type {} = {:?}", name, typ)
                    }
                }
            },
            Object::Return(ref value) => write!(f, "{}", value),
            Object::ResultOk(ref value) => write!(f, "Ok {}", value),
            Object::ResultErr(ref value) => write!(f, "Err {}", value),
            Object::Error(ref value) => write!(f, "{}", value),
            Object::Builtin(ref value) => write!(f, "{:?}", value),
            Object::BuiltinMethod { namespace, method, .. } => write!(f, "{:?}.{}", namespace, method),
            Object::Tuple(elements) => write!(f, "({})", elements.iter().map(|v| v.to_string()).collect::<Vec<String>>().join(", ")),
            Object::Union { type_name, variant, value } => {
                match value {
                    Some(val) => write!(f, "{}({})", variant, val),
                    None => write!(f, "{}: {}", type_name, variant)
                }
            },
        }
    }
}
