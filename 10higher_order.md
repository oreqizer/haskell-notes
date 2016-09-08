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

> **Reminder:** All values are *immutable*. These functions return a *new list* and keep the one that's supplied as it is.

Let's wombo combo these! The following finds a sum of all odd squares less than 10000:

```Haskell
Prelude> sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
166650
Prelude> -- or using a list comprehension:
Prelude> sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])  
166650  
```

The following is a [Collatz conjecture](https://en.wikipedia.org/wiki/Collatz_conjecture):

```Haskell
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)
```

In the interval of 1 to 100, how many sequences are longer than 15?

```Haskell
numLongChains :: Int
numLongChains = length . filter isLong . map chain $ [1..100]
    where isLong xs = length xs > 15
```

### Lambdas

Anonymous functions. Used when we need a one-time function. Syntax:

* `\<parameters> -> <function body>`

Lambdas are often surrounded with parentheses when surrounded by stuff since it's an *expression* returning a *function*. How the `numLongChains` looks with a lambda:

```Haskell
numLongChains :: Int
numLongChains = length . filter (\xs -> length xs > 15) . map chain $ [1..100]
```

### Folds

Also known as **reduce** in some languages. *Folds* take a `Foldable` (`list` for example) and *fold* it to a single value:

```Haskell
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldl (+) 0 -- (+) is basically (\acc x -> acc + x)
```

`acc` is the *accumulator*. That's the current value of the function. It's initial value is defined as the last parameter of the `foldl` function (0 in our case).

> If we wanted the function to work only for lists, the type signature could be `(Num a) => [a] -> a`. `(Foldable t) => t a` is more generic. `[a]` is basically `t a` where `t` is a `list`.

The `foldl` function simply goes and sums the list up, starting with 0 as defined. `foldl` goes from the left to the right, that's why the **l** at the end. The steps after doing `sum' [3,5,2,1]` are:

* acc = 0, x = 3: 0 + 3 = 3
* acc = 3, x = 5: 3 + 5 = 8
* acc = 8, x = 2: 8 + 2 = 10
* acc = 10, x = 1: 10 + 1 = 11

The result is **11**!

There are different variants of *fold* functions:

* `foldl` and `foldr` - fold a list from *l/r* with an accumulator
* `foldl1` and `foldr1` - same as *fold*, except the accumulator is the 1st list element
* `scanl` and `scanr` - as *fold*, but return a `list` with each step's accumulator value
* `scanl1` and `scanr1` - same as *scan*, except the accumulator is the 1st list element

Here's `map` implemented using `foldr`:

```Haskell
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs
```

### Composition & infix application

We've seen these in [#2](./02functions.md) a bit already:

* `.` is function **composition**
* `$` is **infix application** (and is *right associative*)

Function *composition* feeds the result of a function on the previous one. Do essentially doing `f (g x)` is the same as `f . g x`. It's definition:

```Haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

*Infix application* makes the expression on the right to be a parameter to the one on the left. Doing `sum $ map sqrt [1..130]` is the same as `sum (map sqrt [1..130])`. As mentioned, it has the *lowest precedence* of any operator so we can write `sqrt $ 3 + 4 + 9`, which will `(+)` the numbers first, then `sqrt` them. Definition:

```Haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

**Chaining functions:**

Both these operators can be used to get rid of parentheses:

```Haskell
Prelude> succ (head (map (+2) (reverse [1,2,3])))
6
Prelude> succ . head . map (+2) . reverse $ [1,2,3]
6
Prelude> succ $ head $ map (+2) $ reverse [1,2,3]
6
```

However, there's an important difference between the `.` and `$` versions. Besides being more mathematical, `.` makes a *new* function that can exist on it's own and be applied later:

```Haskell
Prelude> :t succ . head . map (+2) . reverse
succ . head . map (+2) . reverse :: (Enum c, Num c) => [c] -> c
```

Trying `:t` on the `$` version will blow up in your face, because it is not a valid expression by itself:

```Haskell
Prelude> :t succ $ head $ map (+2) $ reverse

<interactive>:1:26:
    Couldn't match expected type ‘[r]’ with actual type ‘[a0] -> [a0]’
    Probable cause: ‘reverse’ is applied to too few arguments
    In the second argument of ‘($)’, namely ‘reverse’
    In the second argument of ‘($)’, namely ‘map (+ 2) $ reverse’
```

> `.` is thus suited for creating *new* functions by *composing* smaller ones. `$` is really mostly just for getting rid of parentheses.

One special use-case of `$` is that it can *apply* a value to a function in a higer order one:

```Haskell
Prelude> map ($ 3) [(+3), (*3), (/3)]
[6.0,9.0,1.0]
```
