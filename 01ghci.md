# GHCi

To open interactive mode, type `ghci` to your terminal.

We can use `let` to define a name in the interpreter:

```Haskell
Prelude> let l = [1, 2, 3]
Prelude> l
[1, 2, 3]
```

It's the same as doing `l = [1, 2, 3]` in a module and then loading it.

### Comments

```Haskell
-- Single line comments start with two dashes.

{- Multiline comments can be enclosed
in a block like this.
-}
```
