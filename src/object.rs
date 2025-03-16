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
    Some(Box<Object>),
    None,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

