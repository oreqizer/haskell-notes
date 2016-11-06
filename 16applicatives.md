# Applicatives

Enable applying a function on two or more *functorial values*, where `fmap` is not sufficient. Definition:

```Haskell
class (Functor f) => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Operator `<$>` is an infix `fmap` defined for the use with applicatives. Function `pure` simply puts a value into it's default context. `<*>` applies functorial function on another functor of the same type.

### Examples

Say we have two functor values, `Just 5` and `Just 3`, and we want to add them together. Instead of fiddling with extracting the values manually and checking for `Nothing` and all this, we can use `Maybe` as an `Applicative`:

```Haskell
instance Applicative Maybe where
    pure                  = Just
    (Just f) <*> (Just x) = Just (f x)
    _        <*> _        = Nothing
```

Now, we map the `+` function on the first value, and apply the produced wrapped function onto the other value:

```Haskell
ghci> (+) <$> Just 5 <*> Just 3
Just 8
```

`Applicative` also has one huge benefit. Let's say we are using `Maybe`. Whenever some value for whatever reason failed (thus being `Nothing`), it will get propagated to the end result:

```Haskell
ghci> (+) <$> Nothing <*> Just 3
Nothing
ghci> (+) <$> Just 5 <*> Nothing
Nothing
```

Another good example is nondeterminism. Let's apply a list of *functions* on a list of *values*:

```Haskell
ghci> [(+), (*)] <*> [1, 2] <*> [2, 3]
[3,4,4,5,2,3,4,6]
ghci> [(*3), (+5), (^2)] <*> [2, 3, 4]
[6,9,12,7,8,9,4,9,16]
```

As last time, every possible *function* is applied to every possible *value*, producing a long of all the combinations.

### Function applicative

Functions, `(->) r`, are also instance of `Applicative`. Definition:

```Haskell
instance Applicative ((->) r) where  
    pure x  = \_ -> x
    f <*> g = \x -> f x (g x)
```

`pure` returns a function that simply always returns the argument it was created with:

```Haskell
ghci> pure 3 "kek"
3
```

`<*>` in this case takes two or more *functorial* functions. The results are joined by a mapped function. Returns a function that takes a value, applies it to all functions, and joins the results by the mapped function:

```Haskell
ghci> (+) <$> (+3) <*> (*100) $ 5  
508  -- (5 + 3) + (5 * 100)
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
[8.0,10.0,2.5]
```

### Applicative rules

Like the `Functor` class, `Applicative` has some rules that must be followed for correct implementation:

```Haskell
pure id <*> v = v                            -- Identity
pure f <*> pure x = pure (f x)               -- Homomorphism
u <*> pure y = pure ($ y) <*> u              -- Interchange
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
```

Explanation taken from the [Haskell wiki](https://en.wikibooks.org/wiki/Haskell/Applicative_functors):

* The **identity** rule says that applying the `pure id` morphism does nothing, exactly like with the plain `id` function.
* The **homomorphism** rule says that applying a "pure" function to a "pure" value is the same as applying the function to the value in the normal way and then using `pure` on the result. In a sense, that means `pure` preserves function application.
* The **interchange** rule says that applying a morphism to a "pure" value `pure y` is the same as applying `pure ($ y)` to the morphism. No surprises there - as we have seen in the higher order functions chapter, `($ y)` is the function that supplies `y` as argument to another function.
* The **composition** rule says that if `(<*>)` is used to compose morphisms the composition is associative, like plain function composition.
