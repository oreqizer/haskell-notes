# Functions

Everything in **Haskell** is a function. Types:

* prefix: `succ 9`
* infix: `9 * 10`

Making *prefix* functions *infix*:

```Haskell
> 3 `elem` [1, 3, 3, 7]  -- surround function name with ``
True
```

Making *infix* functions *prefix*:

```Haskell
> (+) 2 5  -- surround function with parentheses
7
```
