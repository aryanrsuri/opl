# oPL ("Opal") Language Specification
## Abstract

This technical standard defines the specification for the oPL (pronounced "opal") programming language. Including a list of examples and basic usage.

## Lexical Structure

```
pub enum Token {
    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    Type,
    Match,
    With,
    Of,
    Raise,

    // Primitive
    Char,
    String,
    Int,
    Float,
    Boolean,

    // Algebraic
    List,
    Union,
    Record,
    Option,
    Result,
    Ok,
    Error,
    Unit,
    Some,
    None,

    // Literals
    Identifier(String),
    StringLiteral(String),
    IntegerLiteral(String),
    FloatLiteral(String),
    Comment(String),

    // Parsing
    End,
    Illegal,

    // Operators
    Plus,         // +
    Minus,        // -
    Concat,       // ++
    Product,      // *
    ForwardSlash, // /
    Assign,       // =
    Bang,         // !
    Underscore,   // _
    Comma,        // ,
    Equal,        // ==
    DoesNotEqual, // !=
    GreaterThan,  // >
    LessThan,     // <
    GTOrEqual,    // >=
    LTOrEqual,    // <=
    Vbar,         // |
    Pipe,         // |>
    Arrow,        // ->
    Modulo,       // %
    Ampersand,    // &
    Caret,        // ^
    Polymorph,    // 'a

    // Delimiters
    LeftBrace,    // {
    RightBrace,   // }
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    Colon,        // :
    Cons,         // ::
    SemiColon,    // ;
    Period,       // .
    Over,         // ..
}
```

## Primitive Types

- Int: Signed 64-bit width.
- Float: Signed 64-bit width.
- Char: Unsigned 8-bit width.
- Boolean: False or True
- String: Sequence of Chars

## Expression Binding

Expression Binding (Immutable by default):
Syntax: `let <identifier> = <expression>;`

```
let x = 10;
let y = 20;
let x_y = x + y;
let hello = "Hello, "
let world = "World!"
let hello_world = hello ++ world;
```

## Functions

A function in oPL takes one or more arguments and returns a result. A function can be defined using the `fn` keyword.

Syntax: `let <identifier> = fn 'a, 'b, ... -> <expression>;`

Functions can be evaluated using the syntax: `<function_identifier> <argument>`
Example:

```
let sum = fn x, y -> x + y;
let multi_line = fn x -> {
  let shadow = x;
  x + 400;
};

multi_line 10;  // Function application with 10 as the argument.
```

## Function Piping

This operator is used to pipe the result of one expression into the next. It is closely related to function composition.

Syntax: `<expression> |> <expression>`
Example:

```
let list_of_ints = parse filepath
  |> buffer_to_lines
  |> extract_int_from_lines
  |> collect_to_list;
```
## Control Flow:

If Expressions: Conditional expressions in oPL are expressions themselves, ensuring both branches return a value.

Syntax: `if <condition> { <expression> } else { <expression> }`

Example:
```
let ReLU = fn x -> if x > 0 { x } else { 0 };
let divide = fn x, y -> if y == 0 { Error } else { Ok x / y };
```

## Function Piping

Syntax: `<expression> |> <expression>`
Example:

```
let list_of_ints = parse(filepath)
  |> buffer_to_lines
  |> extract_int_from_lines
  |> collect_to_list;
```

## Control Flow

If Expressions: Conditional expressions.

Syntax: `if <condition> { <expression> } else { <expression> }`
Example:

```
let ReLU = fn x -> if x > 0 { x } else { 0 };
let real = fn x -> if x > 0 { Ok x } else { None };
let divide = fn x, y -> if y == 0 { Error } else { Ok x / y };
let factorial = fn n -> if n == 0 { 1 } else { n * factorial (n - 1) };

```

## List

A list is a collection of disparate elements of a single type 'a

Syntax: `let <identifier> = [ 'a, 'a, 'a ];`
Example:
```
let l0= [1, 2, 3, 4];
let l1 = 0 :: l0;
let second_element = match l0 with
  | x :: y :: u -> Some y
  | x :: u -> Some x
  | [] :: None
  ;
```

### Tagged Union

A tagged union can be one of several variants which can either hold a value of some type 'a or not.

Syntax: type `<identifier> = | <variant> of <type> | ... ;`
Example:
```
type Cell =
  | Alive of Int
  | Dormant
  ;
```

#### Option (Polymorphic Tagged Union)

The Option type is used to represent an optional value. It may either hold a value of type 'a (`Some`) or no value (`None`).

Example
```
let value = Some 45;
let absent = None;
match Some 42
| Some x -> x;
| None -> 0 // Default value when None
;
```
#### Result (Polymorphic Tagged Union)

A result type can express an outcome as either a success `Ok` of type 'a or a failure `Error` of type 'b

Example
```
Ok 42;
Error "Bruh";
```

### Record

A Record is a product type (sequence of elements of any types 'a * 'b ... 'n) but identified by a key insted of its index (i.e a tuple). fields can be accessed using the dot notation `record_identifier.field_key`

Syntax:
```
type <identifier> = {
  field_key_1: value_1,
  field_key_2: value_2,
}
```

Example
```
type Flashcard = {
  flashcard_id: int,
  front: string,
  back: string,
  tags: string list,
};

let flashcard = {
  flashcard_id = 1,
  front = "front",
  back = "back",
  tags = ["first", "metaphysics"],
};

flashcard.back;
```

### Parametric Polymorphism

Tagged unions and records, using the `type` keyword, can be parametrized by any valid other type `'a`. This is known in many languages as a _generic_.

Syntax
(Tagged Union) `type 'a identifier = | variant ...;`
(Record)` type 'a identifier = { ... };`

Example
```
type 'a Option =
  | Some of 'a
  | None
;

type 'a Coordinates = {
  x: 'a,
  y: 'a,
  z: 'a,
};

let player_1 = {x = 100, y = 100, z = 0};
```


## Pattern Matching

Matching is a powerful data inspection protocol

Syntax: `match <expression> with | <pattern> -> <expression> | ... ;`
Example:

```
type Cell =
  | Alive of Int
  | Dormant
  ;

let partition = fn cell -> {
  match cell with
    | Alive n -> "This cell is alive with " ++ string_of_int n ++ " health"
    | Dormant n -> "This cell is dormant"
    | _ -> "Unknown cell state"
  ;
};

let curr_cell = Alive 10;
partition curr_cel;
```
