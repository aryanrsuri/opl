use crate::object::Object;

pub fn from_int_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("from_int expects exactly one argument".to_string());
    }

    match &args[0] {
        Object::Integer(n) => Object::String(n.to_string()),
        _ => Object::Error("from_int expects an integer argument".to_string()),
    }
}

pub fn from_float_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("from_float expects exactly one argument".to_string());
    }

    match &args[0] {
        Object::Float(n) => Object::String(n.to_string()),
        _ => Object::Error("from_float expects a float argument".to_string()),
    }
}

pub fn from_bool_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("from_bool expects exactly one argument".to_string());
    }

    match &args[0] {
        Object::Boolean(b) => Object::String(b.to_string()),
        _ => Object::Error("from_bool expects a boolean argument".to_string()),
    }
}

pub fn length_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("length expects exactly one argument".to_string());
    }

    match &args[0] {
        Object::String(s) => Object::Integer(s.len() as i128),
        _ => Object::Error("length expects a string argument".to_string()),
    }
}

pub fn split_builtin(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error("split expects exactly two arguments".to_string());
    }

    match &args[0] {
        Object::String(text) => {
            match &args[1] {
                Object::String(delimiter) => {
                    let split_string = text.split(delimiter).collect::<Vec<&str>>();
                    Object::List(split_string.iter().map(|s| Object::String(s.to_string())).collect())
                }
                _ => Object::Error("split expects a string argument".to_string()),
            }
        }
        _ => Object::Error("split expects a string argument".to_string()),
    }
}

