#### Lists

Lists are an ordered seuqence of elements all of type 'a

```
let l = [1,2,3];
-- list * int -> [1,2,3]
```


#### Inbuilt functions

Opl supports several higher order functions commong in FPLs

``` 
-- map  : (a -> b) -> [a] -> [b]
-- filter : (a -> bool) -> [a] -> [a]
-- fold  : (b -> a -> b) -> b -> [a] -> b
-- any : (a -> bool) -> [a] -> bool
-- all : (a -> bool) -> [a] -> bool

let sq = fn x -> x*x;
map(sq, [1,2,3,4]);
-- list * int -> [1,4,9,16];

let three = fn x -> x > 3;
filter(three, [3,0,-1,10,20]);
-- list * int -> [10,20];
```