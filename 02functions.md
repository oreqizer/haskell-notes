# Functions

Everything in **Haskell** is a function. Types:

* prefix: `succ 9`
* infix: `9 * 10`

Function *application* has precedence over infix functions:

```Haskell
Prelude> succ 9 + 10
20
```

> If a function is comprised only of *special characters*, it's considered an infix function by default. `==`, `*`, `+`, `-`, `/` and many more are all infix.

Infix functions have their own precedence rules (mostly related to math):

```Haskell
Prelude> 10 * 9 + 8
98
Prelude> 10 + 9 * 8
82
```

Making *prefix* functions *infix*:

```Haskell
Prelude> 3 `elem` [1, 3, 3, 7]  -- surround function name with ``
True
```

Making *infix* functions *prefix*:

```Haskell
Prelude> (+) 2 5  -- surround function with parentheses
7
```

### Pattern matching

Function bodies can be defined for different *patterns*:

```Haskell
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"
```

This is used mainly to define edge cases in **recursive** functions:

```Haskell
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)
```

> Pattern matching can lead to an error if no pattern is matched during execution.

**Matching tuples:**

Tuples can be matched as a whole:

```Haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
```

Or *destructured*:

```Haskell
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```

We can do it also in list comprehensions, `[a+b | (a,b) <- xs]`.

**Matching lists:**

Due to `[1,2,3]` being just syntactic sugar for `1:2:3:[]`, lists can be matched like so:

```Haskell
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs
```

**As pattern:**

As pattern is defined as `name@` before a pattern, where *name* will be the matched pattern as a whole:

```Haskell
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]
```

*Note:*
`++` cannot be used for pattern matching.

### Guards

Guards test for first expression that evaluates to `True`. The `otherwise` guard is optional:

```Haskell
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"
```

### Where

The `where` keyword allows **1-n** local helper function declaration, that has access to all of it's parent's content:

```Haskell
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0
```

### Let
