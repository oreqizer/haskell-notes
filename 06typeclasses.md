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

*Note:*
Read, due to it's signature of `read :: (Read a) => String -> a` cannot be used alone (it results in an error). The compiler doesn't know which *type* we want in return. Supplying the result to a function makes it **infer** a type, or we can use an explicit **type annotation**:

```Haskell
Prelude> read "1.1" :: Float
1.1
```

* `Enum` is for sequentially ordered types. Allows the use of *list ranges*, `succ` and `pred`. Types in this class: `Bool`, `Char`, `Ordering` and numbers.
* `Bounded` members have lower and upper bound, `minBound` and `maxBound` respectively.
* `Num` describes all numbers.
* `Integral` are whole numbers. To get `Num`, use `fromIntegral`.
* `Floating` are floating point numbers, duh.
