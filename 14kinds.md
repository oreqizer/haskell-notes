# Kinds

A *type* of a *type*. Can be checked with `:k`.

Kinds show whether a type is a *concrete type*, or a *type constructor*:

```Haskell
ghci> :k Int
Int :: *  -- a concrete type
ghci> :k Maybe
Maybe :: * -> *  -- a type constructor with one type parameter
ghci> :k Either
Either :: * -> * -> *  -- a type constructor with two type parameters
```

We can also have type constructors that take other type constructors as parameters:

```Haskell
data Barry t k p = Barry { yabba :: p, dabba :: t k }
```

Notice in `dabba`, `k` is applied to `t`, thus the *kind* is `(* -> *)`. `k` and `p` themselves are concrete types:

```Haskell
ghci> :k Barry  
Barry :: (* -> *) -> * -> * -> *
```
