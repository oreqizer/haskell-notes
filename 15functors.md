# Functors

Things that can be mapped over. Definition:

```Haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

Notice that `f` has the *kind* of `* -> *`. Functors describe how `fmap` should behave when used on different *type constructors*.

> To understand functors more easily, one could think about the type constructor as a *box* that holds a value of a specific type. `fmap` takes a regular function of type `a -> b`, a *box* `f` that holds a value of type `a` in the form `f a`, applies the function to `a` and puts it back in the box as `f b`. However, it's more correct to say that a functor is a **computational context**.

For example, lists are a *computational context* of nondeterminism. We don't know what value to apply `fmap` to, so we just apply it to every possible value. If we wanted to define `fmap` for lists, it would be defined like so:

```Haskell
instance Functor [a] where
    fmap _ [] = []
    fmap f (x:xs) = f x : fmap xs
```

This is similar to how `map` is defined, and in reality, `fmap` is defined simply as `fmap = map`.

> **Note:** Originally, `fmap` was called `map`. This proved to be a difficulty when junior people were learning about lists, as the error messages were too abstract and cryptic. `map` was thus left for lists only, and functor's `map` was renamed to `fmap`.

`Maybe`'s `fmap` implementation is pretty straightforward:

```Haskell
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap _ Nothing = Nothing
```

If the value with context is a `Just`, it applies the function on the underlying value. Else, just return `Nothing` since we have nothing to map over. You get the point.

### Function functor

Functions, `(->) r`, are also instances of `Functor` typeclass. Definition:

```Haskell
instance Functor ((->) r) where  
    fmap = (.)  -- more verbose version: fmap f g = f . g
```

It's really just a function composition.

Again, type of `fmap` is `fmap :: (a -> b) -> f a -> f b`. If we replace `f` with `(->) r`, we get `fmap :: (a -> b) -> ((->) r a) -> ((->) r b)`, which is the same as `fmap :: (a -> b) -> (r -> a) -> (r -> b)`.

### Functor rules

These are rules essential for correct functor implementation:

> **First rule:** Applying `id` function on a functor has to return the same functor.

```Haskell
fmap id = id
```

> **Second rule:** Composing two functions and mapping them on a functor should yield the same result as mapping the first function, then mapping the second one on the result.

```Haskell
fmap (g . f) = fmap g . fmap f
```

### Explanation

Functors, also called *morphisms* in category theory, are a type of mapping between categories.

> **Note:** Mathematical mumbo jumbo incoming! Only read this if you really want to understand functors as an abstract concept. You don't need this if you just want to know and use Haskell's `Functor` typeclass.

Haskell's main category we will be discussing is **Hask**. Haskell types are it's objects, `.` is *composition*, a function `f :: a -> b` for types `a` and `b` is a *morphism*. In **Hask**, the *identity morphism* is `id`, so we get:

```
id . f = f . id = f
```

For this to work, we're missing subscripts. Function `id` in Haskell is *polymorphic* - it has many sources and many outputs. This conflicts with category theory's morphisms, as they are *monomorphic*, they have a specific source and a specific target object. Haskell deals with this by specifying the **type** of the function, thus making *polymorphic* functions *monomorphic* for the type. This is important for us to allow discussion about functions as they were monomorphic.

Functors in Haskell are mappings from the main **Hask** category, to it's subcategory that is a category of types of the *type constructor*. For example, the list functor goes from **Hask** to **Lst**, where **Lst** is the category containing only *list types*.

To sum up, check this list from the [Haskell wiki](https://en.wikibooks.org/wiki/Haskell/Category_theory):

* We work in the category **Hask** and its subcategories.
* Objects are types.
* Morphisms are functions.
* Things that take a type and return another type are type constructors.
* Things that take a function and return another function are higher-order functions.
* Typeclasses, along with the polymorphism they provide, make a nice way of capturing the fact that in category theory things are often defined over a number of objects at once.
