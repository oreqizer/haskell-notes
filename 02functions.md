# Functions

Everything in **Haskell** is a function. Types:

* prefix: `succ 9`
* infix: `9 * 10`

Function *application* has precedence over infix functions:

```Haskell
Prelude> succ 9 + 10
20
```

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
