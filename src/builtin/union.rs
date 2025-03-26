use crate::object::Object;

// Check if a union object is the same variant as another variant object
pub fn is_variant_builtin(args: Vec<Object>) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "Wrong number of arguments for is_variant: expected 2, got {}",
            args.len()
        ));
    }

    match (&args[0], &args[1]) {
        (Object::Union { variant: variant1, .. }, Object::Union { variant: variant2, .. }) => {
            // Compare the variant names only, ignore the values
            Object::Boolean(variant1 == variant2)
        },
        (_, Object::Union { .. }) => {
            Object::Error(format!("First argument must be a union variant, got {:?}", args[0]))
        },
        (_, _) => {
            Object::Error(format!("Second argument must be a union variant, got {:?}", args[1]))
        }
    }
}

// Get the type name of a union variant
pub fn type_of_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments for type_of: expected 1, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Union { type_name, .. } => {
            Object::String(type_name.clone())
        },
        _ => {
            Object::Error(format!("Expected union variant, got {:?}", std::mem::discriminant(&args[0])))
        }
    }
}

// Get the variant name as a string
pub fn variant_name_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments for variant_name: expected 1, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Union { variant, .. } => {
            Object::String(variant.clone())
        },
        _ => {
            Object::Error(format!("Expected union variant, got {:?}", std::mem::discriminant(&args[0])))
        }
    }
}

// Check if a union object has a value (not a Unit variant)
pub fn has_value_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments for has_value: expected 1, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Union { value, .. } => {
            Object::Boolean(value.is_some())
        },
        _ => {
            Object::Error(format!("Expected union variant, got {:?}", std::mem::discriminant(&args[0])))
        }
    }
}

// Extract the value from a variant, or error if none exists
pub fn value_of_builtin(args: Vec<Object>) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "Wrong number of arguments for value_of: expected 1, got {}",
            args.len()
        ));
    }

    match &args[0] {
        Object::Union { value: Some(val), .. } => {
            (**val).clone()
        },
        Object::Union { variant, value: None, .. } => {
            Object::Error(format!("Variant {} has no value", variant))
        },
        _ => {
            Object::Error(format!("Expected union variant, got {:?}", std::mem::discriminant(&args[0])))
        }
    }
} 