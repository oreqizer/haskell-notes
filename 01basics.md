# Basics of Haskell

* **Statically typed** with **type inference**.
* **Purely functional** - there are no *statements*, nor *instructions*, only *expressions*.
* Everything is **immutable**.
* Uses **lazy evaluation**.

### GHCi

To open interactive mode, type `ghci` to your terminal.

We can use `let` to define a name in the interpreter:

```Haskell
Prelude> let l = [1, 2, 3]
Prelude> l
[1, 2, 3]
```

It's the same as doing `l = [1, 2, 3]` in a module and then loading it.

`Prelude` is the basic module that's loaded by default to all *GHC* modules (unless explicitly imported, or prevented by a language extension).

**Useful commands:**

* `:t <expression>` checks the type of an expression
* `:l <module>` loads a module, be it a standard one, or a file
* `:r` reloads the last loaded module
* `:q` quits `ghci`

### Comments

```Haskell
-- Single line comments start with two dashes.

{- Multiline comments can be enclosed
in a block like this.
-}
```
