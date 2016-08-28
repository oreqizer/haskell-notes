# Types

**Haskell**'s biggest strength! `:t` shows a type of an expression:

```Haskell
Prelude> :t "HELLO!"  
"HELLO!" :: [Char]
```

It prints `expression :: type`, where `::` is read as *has type of*. It is also used for type casting, when applicable:

```Haskell
Prelude> :t 1
1 :: Num a => a
Prelude> :t 1 :: Integer
1 :: Integer :: Integer
Prelude> :t 1 :: Float
1 :: Float :: Float
```

Function types:

```Haskell
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z
```

They can be also determined by `ghci` from the implementation (checked with `:t`).

### Common types

* `Int` bounded integer (# of bits depends on the machine)
* `Integer` unbounded integer, can be infinitely long
* `Float` single precision float number
* `Double` double precision float number!
* `Char` is a character in single quotes
* `Bool` is `True` or `False`

### Type variables

Used to describe **polymorphic functions**:

```Haskell
Prelude> :t head
head :: [a] -> a
```

`a` can be any type. `head` takes a list of items of type `a`, returns an item of the same type `a`.
