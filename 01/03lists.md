# Lists

Lists are **homogeneous** - all elements must be of the same type.

### Joining

`++`: concatenation

```Haskell
> [1, 2] ++ [3, 4, 5]
[1,2,3,4,5]
```

> Careful when using the ++ operator. Putting together two lists internally, Haskell has to walk through the whole list on the left side of ++. Prefer : if available.

`:`: unshift

```Haskell
> 1 : [2, 3]
[1,2,3]
```

*Note:*
`[1,2,3]` is actually just syntactic sugar for `1:2:3:[]`.

### Extracting

`!!`: get element at index

```Haskell
> [1, 2, 3, 4] !! 2
3
```

### Comparing

Lists containing *comparable* elements can themselves be compared:

```Haskell
> [3,2,1] > [2,1,0]  
True  
> [3,2,1] > [2,10,100]  
True  
> [3,4,2] > [3,4]  
True
```

### Partitioning

```Haskell
> head [5,4,3,2,1]  
5
```

```Haskell
> tail [5,4,3,2,1]  
[4,3,2,1]
```

```Haskell
> last [5,4,3,2,1]  
1
```

```Haskell
> init [5,4,3,2,1]  
[5,4,3,2]
```

Here's a nice helper from the [book](https://learnyouahaskell.com):

![listmonster](../assets/listmonster.png)

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
> [1..10]
[1,2,3,4,5,6,7,8,9,10]
```

Defining steps (must be regular):

```Haskell
> [2, 4..10]
[2,4,6,8,10]
```
