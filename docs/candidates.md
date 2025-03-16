#### List Comprehension

Equivilant to Haskell 
```
let even = fn n -> {
    if n % 2 == 0 { true }
    else { false }
};

let evens = [x | x <- [1,2,3,4,5,6,7,8,9,10] , even x];
-- list * int -> [2,4,6,8,10]

```

#### Product of product types

Product types of product types are nested in a parenthesis
```
type tags = option * (list * string);
-- option * (list * string) -> option * (list * string)
```

#### Records field access

Given a record declaration:
```
type Flashcard = {
    front: string,
    back: string,
    tags: list * string,
};

let flashcard1 = {
    front = "What is first letter of the alphabet?",
    back = "a",
    tags = ["lexicon", "nomenclature"],
};
```

Accessing a field should be done using a dot, `.`
```
let f = flashcard1.front;
-- string -> "What is the first letter of the alphabet?"
```

#### Pattern matching

Pattern matching is the most powerful feature in opl

Match an expression to an expression

```
-- Unwrap the value
match Some 42 with
| Some x -> x;
| None -> 0 
;
```

Match on tagged unions
```
type Light = 
    | Red
    | Yellow
    | Green
    ;

let next_light = match light with
    | Red -> Green
    | Yellow -> Red
    | Green -> Yellow
    ;
```

-- Match records to access values


#### Polymorphic types