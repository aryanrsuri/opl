use crate::object::Object;

pub fn println_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("println expects exactly one argument".to_string());
    }

    match &args[0] {
        Object::String(s) => {
            println!("{}", s);
            Object::Unit
        }
        _ => Object::Error("println expects a string argument".to_string()),
    }
}

