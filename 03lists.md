# Lists

Lists are **homogeneous** - all elements must be of the same type.

### Joining

`++`: concatenation

```Haskell
ghci> [1, 2] ++ [3, 4, 5]
[1,2,3,4,5]
```

> Careful when using the `++` operator. Putting together two lists internally, Haskell has to walk through the whole list on the left side of `++`. Prefer `:` if available.

`:`: unshift

```Haskell
ghci> 1 : [2, 3]
[1,2,3]
```

More on the definition of lists in [#12](./12data_types.md).

### Extracting

`!!`: get element at index

```Haskell
ghci> [1, 2, 3, 4] !! 2
3
```

### Comparing

Lists containing *comparable* elements can themselves be compared:

```Haskell
ghci> [3,2,1] > [2,1,0]  
True  
ghci> [3,2,1] > [2,10,100]  
True  
ghci> [3,4,2] > [3,4]  
True
```

### Partitioning

```Haskell
ghci> head [5,4,3,2,1]  
5
```

```Haskell
ghci> tail [5,4,3,2,1]  
[4,3,2,1]
```

```Haskell
ghci> last [5,4,3,2,1]  
1
```

```Haskell
ghci> init [5,4,3,2,1]  
[5,4,3,2]
```

Here's a nice helper from the [book](http://learnyouahaskell.com):

![listmonster](assets/listmonster.png)

### Other

More useful functions:

* `null` checks for empty list
* `length` returns the list length
* `reverse` reverses the list
* `take` takes first *n* elements
* `drop` drops first *n* elements
* `elem` checks for element existence
* `cycle` cycles the list to infinty
* `repeat` takes an element and repeats it to infinity

Mathematical:

* `maximum`
* `minimum`
* `sum`
* `product`

### Range

Ranges are a way of making lists that are arithmetic sequences of elements that can be *enumerated*.

```Haskell
ghci> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

Defining steps (must be regular):

```Haskell
ghci> [2, 4..10]
[2,4,6,8,10]
```

### List comprehension

Much like *set comprehension* in mathematics, `S = {2 * x | x ∈ N, x <= 10}`. The same in **Haskell**:

```Haskell
ghci> [x * 2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]
```

**Common structure:**

`[mapper | source, ...sources | filters]`

Adding more *sources* results in all possible combinations. All *filters* must be passed for the element to be accepted.

*More sources:*

```Haskell
ghci> [x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  
[55,80,100,110]
```

*Filter:*

```Haskell
ghci> [x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]
```

*Alphabetical:*

```Haskell
ghci> let justUpper st = [c | c <- st, c `elem` ['A'..'Z']]
ghci> justUpper "lOL oMG Wtf"
"OLMGW"
```
