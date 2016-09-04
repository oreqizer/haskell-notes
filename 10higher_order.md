# Higher order functions

Functions that *take* and/or *return* other functions:

```Haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f
```

`applyTwice` takes a function and applies it on an argument twice:

```Haskell
Prelude> applyTwice (*3) 3
27
```

There are many higher order functions in the standard library, including `map`, `filter`, `foldr`, `flip`, `zip`, `zipWith`. One example:

```Haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f y x = f x y
```

### Map & filter

These are some of the most widely used, also in other languages supporting first-class functions and generics (be it *JavaScript*, *C#*, *Java*...).

**Definitions:**

```Haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

`map` takes a function and applies it to every element in a list:

```Haskell
Prelude> map (+3) [1,5,3,1,6]
[4,8,6,4,9]
```

`filter` takes a function taking an element and returning a `Bool`. If `True`, the element stays in the list. If `False` it is omitted:

```Haskell
Prelude> filter (>3) [1,5,3,2,1,6,4,3,2,1]
[5,6,4]
```

> **Reminder:** All **Haskell** values are *immutable*. These functions return a *new list* and keep the one that's supplied as it is.
