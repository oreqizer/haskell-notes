# Currying

Props to [Haskell Curry](https://en.wikipedia.org/wiki/Haskell_Curry). Make sure you understand this stuff!

> **Note:** It's still common to say a function takes multiple parameters despite what is going on under the hood.

Every function in **Haskell** is *curried* by default - officially takes 1 parameter. We can perform **partial application**:

```Haskell
ghci> :t max
max :: Ord a => a -> a -> a
ghci> :t max 1337
max 1337 :: (Num a, Ord a) => a -> a
```

Partially applying a function creates a *new* function we can later call with the missing parameters. These are equivalent:

```Haskell
ghci> max 4 5
5  
ghci> (max 4) 5
5
```

> Putting a space between two things is simply **function application**. Definition of `max` can be written as `max :: (Ord a) => a -> (a -> a)`. That could be read as: `max` takes an `a` and returns (that's the `->`) a function that takes an `a` and returns an `a`.

*Trailing parameters* can be omitted due to partially applied functions creating new functions:

```Haskell
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100 -- same as: compareWithHundred x = compare 100 x
```

*Infix* functions can be partially applied. Applying a value to the function will put it on the **missing side**:

```Haskell
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10) -- same as: divideByTen x = x/10
```

Switching sides:

```Haskell
ghci> :t (/10)
(/10) :: Fractional a => a -> a
ghci> (/10) 5
0.5
ghci> :t (10/)
(10/) :: Fractional a => a -> a
ghci> (10/) 5
2.0
```

Which side of function to partially apply can also be chosen on *prefix* functions made *infix*:

```Haskell
ghci> :t (`elem` ['A'..'Z'])
(`elem` ['A'..'Z']) :: Char -> Bool
ghci> :t (['A'..'Z'] `elem`)
(['A'..'Z'] `elem`) :: Foldable t => t [Char] -> Bool
```

If both sides aren't applied, e.g. by doing `(/)`, left side is applied first. This is due to the type:

```Haskell
ghci> :t (/)
(/) :: Fractional a => a -> a -> a
ghci> :t (/) 10
(/) 10 :: Fractional a => a -> a
ghci> (/) 10 2
5.0
```
