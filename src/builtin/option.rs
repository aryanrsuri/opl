use crate::object::Object;

pub fn unwrap_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error("Expected 1 argument".to_string());
    }

    let arg = args[0].clone();
    match arg {
        Object::OptionSome(value) => *value,
        Object::OptionNone => Object::Error("Expected Some, got None".to_string()),
        _ => Object::Error("Expected Option, got ".to_string() + &arg.to_string()),
    }
}