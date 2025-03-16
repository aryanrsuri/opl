#### Pattern matching

```
match Some 42 with
| Some x -> x;
| None -> 0 // Default value when None
;


#### List Comprehension

```
let evens = [x | x <- [1..10] , even x] 
-- evens: list * int
```