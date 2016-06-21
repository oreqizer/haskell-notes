# Functors

Functor is a `Prelude` class for types which can be mapped over. It has a single method, called `fmap`.

```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Functor laws:

> The first functor law states that if we map the `id` function over a functor, the functor that we get back should be the same as the original functor.

```Haskell
fmap id = id
```

> The second law says that composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.

```Haskell
fmap (g . f) = fmap g . fmap f
```

# Applicatives

Applicative typeclass lies in the `Control.Applicative` module and it defines two methods, `pure` and `<*>`. It doesn't provide a default implementation for any of them, so we have to define them both if we want something to be an applicative functor. The class is defined like so:

```Haskell
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
