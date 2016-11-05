# Functions

Everything that takes a parameter in **Haskell** is a function.

* A *value*: `pi = 3.14`
* A *function*: `doubleMe x = x * 2`

Functions are values of *function* type.

**Types of functions:**

* prefix: `succ 9`
* infix: `9 * 10`

Function *application* is **left associative** and has precedence over infix functions:

```Haskell
ghci> succ 9 + 10
20
```

> If a function is comprised only of *special characters*, it's considered an infix function by default. `==`, `*`, `+`, `-`, `/` and many more are all infix.

Making *prefix* functions *infix*:

```Haskell
ghci> 3 `elem` [1, 3, 3, 7]  -- surround function name with ``
True
```

Making *infix* functions *prefix*:

```Haskell
ghci> (+) 2 5  -- surround function with parentheses
7
```

> Mind: the above is partial application (see [#9](./09currying.md)), not a syntactic construct. `2` gets applied to the left, producing `(2+)`. Then `5` gets applied, making it `2 + 5` which evaluates to `7`.

### Precedence

Precedence determines what gets evaluated first.

* Function *application* has a level of **10**.
* Infix functions range from **0 - 9**, and obey math rules where applicable.

```Haskell
ghci> 10 * 9 + 8
98
ghci> 10 + 9 * 8
82
```

Function *composition* (`.`) has a level of **9**, while *infix application* (`$`) has a level of **0**.

```Haskell
ghci> take 1 . reverse . map (*3) $ [1,2,3]
[9]
```

> Composing functions in the context of `f . g x` means calling `f` on the result of `g x`, thus being equivalent with `f (g x)`.

In the example, *composition* takes place first, creating a function with signature `Num a => [a] -> [a]`, and `$` applies `[1,2,3]` to it.

The `$` is needed, because otherwise:

* `map (*3) [1,2,3]` evaluates to `[3,6,9]`
* `reverse .` wants a function, but gets `[3,6,9]` - blows up
