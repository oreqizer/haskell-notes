# Typeclasses

An *interface* that defines some behavior. If a *type* is a part of a *typeclass*, it supports the behavior the typeclass describes.

**Class constraint** describes instance of what typeclass does a type have to be. Multiple typeclasses can be specified, separated by comma. It's located before the *type signature*, followed by a `=>`:

```Haskell
ghci> :t (==)  
(==) :: (Eq a) => a -> a -> Bool
```

### Common typeclasses

* `Eq` describes equality testing
* `Ord` describes comparables. Types need to implement `Eq` first.
* `Show` allows displaying values as strings
* `Read` is basically the opposite of `Show`:

```Haskell
Prelude> read "8.2" + 3.8
12.0
```

**Note:** Read, due to it's signature of `read :: (Read a) => String -> a` cannot be used alone (it results in an error). The compiler doesn't know which *type* we want in return. Supplying the result to a function makes it **infer** a type, or we can use an explicit **type annotation** as `read "1.1" :: Float`.

* `Enum` is for sequentially ordered types. Allows the use of *list ranges*, `succ` and `pred`. Types in this class: `Bool`, `Char`, `Ordering` and numbers.
* `Bounded` members have lower and upper bound, `minBound` and `maxBound` respectively.
* `Num` describes all numbers.
* `Integral` are whole numbers. To get `Num`, use `fromIntegral`.
* `Floating` are floating point numbers, duh.

### Instances

Writing an *instance* of a *typeclass* is pretty straightforward:

```Haskell
data TrafficLight = Red | Yellow | Green

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"
```

Now we can use it:

```Haskell
Prelude> show Red
"Red light"
```

We can write typeclasses that are *subclasses* of other ones:

```Haskell
class (Eq a) => Num a where
    ...
```

Only *concrete types* (see [#12](./12data_types.md)) can be made instances of a typeclass. We can't, for example, make `Maybe` an instance of a typeclass, because it is a *type constructor*. We can, however, make a generic `instance`:

```Haskell
instance (Eq m) => Eq (Maybe m) where  
    Just x == Just y = x == y  
    Nothing == Nothing = True  
    _ == _ = False
```

Here all types in the form `Maybe m`, where `m` is an instance of `Eq`, get the `instance`.

### Creating a typeclass

Say we want to emulate *JavaScript*'s truthy value behavior (`""`, `null` and `0` are *falsy*, `["kek"]` is *truthy* etc.):

```Haskell
class Truthy a where
    truthy :: a -> Bool
```

This defines the function `truthy` for us. Now, we have to make some common types an `instance` of the typeclass:

```Haskell
instance Truthy Int where
    truthy 0 = False
    truthy _ = True

instance Truthy [a] where
    truthy [] = False
    truthy _ = True

instance Truthy (Maybe m) where
    truthy Nothing = False
    truthy _ = True
```

Now we can use it:

```Haskell
Prelude> truthy ""
False
Prelude> truthy Nothing
False
Prelude> truthy $ Just 6
True
```
