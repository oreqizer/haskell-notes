# Functions

Everything that takes a parameter in **Haskell** is a function.

* A *value*: `pi = 3.14`
* A *function*: `doubleMe x = x * 2`

Functions are values of *function* type.

**Types of functions:**

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

This is often used to define edge cases in **recursive** functions:

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

Putting `name@` before a pattern where *name* will be the matched pattern as a whole:

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

The `where` keyword allows defining **1-n** local helper *functions* or *names*:

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

The form is `let <bindings> in <expression>`. More local than `where` - not available across *guards* or *matched patterns*.

```Haskell
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea
```

Let bindings are *expressions*, `where` is a *syntactic construct*.

```Haskell
Prelude> [let square x = x * x in (square 5, square 3, square 2)]
[(25,9,4)]
```

Multiple inline `let` bindings can be separated by `;`:

```Haskell
Prelude> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)
(6000000,"Hey there!")
```

**Pattern matching:**

```Haskell
Prelude> (let (a,b,c) = (1,2,3) in a+b+c) * 100
600
```

**List comprehensions:**

```Haskell
calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]
```

The `in` part of the `let` binding is omitted when used in *list comprehensions* because the visibility of the names is already predefined there.

### Case expressions

Used for matching *patterns* to *results*:

```Haskell
case expression of pattern1 -> result1
                   pattern2 -> result2
                   pattern3 -> result3
```

`expression` is matched against the *patterns*. The first matching *pattern*'s *result* is the value of the `case` expression.

> Function pattern matching is just a syntactic sugar for `case` expression.

The following examples are equal:

```Haskell
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."
```
