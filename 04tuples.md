# Tuples

Tuples are *heterogeneous*. Each tuple has it's **shape** determining it's type:

```Haskell
Prelude> :t (1, "kek")
(1, "kek") :: Num t => (t, [Char])
```

Tuples of different types thus cannot be combined, for example in a list.

### Functions

These work only on tuples of two elements:

* `fst` takes the first element
* `snd` takes the second element

Zipping with `zip`:

```Haskell
Prelude> zip [1..5] ["one", "two", "three", "four", "five"]
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]
```

## Exercise:

Which right triangle that has integers for all sides and all sides equal to or smaller than 10 has a perimeter of 24?

```Haskell
Prelude> let triangles = [(a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10]]
Prelude> -- make b smaller than hypothenuse and c smaller than b, make the triangle right
Prelude> let rightTriangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]
Prelude> -- add the condition we are looking for
Prelude> let wantedTriangles = [(a,b,c) | (a,b,c) <- rightTriangles, a+b+c == 24]
Prelude> wantedTriangles
[(6,8,10)]
```
