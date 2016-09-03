# Recursion

Declarative way of doing repeated stuff. Pillars of *recursive functions*:

* determine **edge condition**
* call function from within itself

```Haskell
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)
```

Cliché example - **quicksort**:

```Haskell
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted
```

> Yes, this isn't in-situ. But it's nice and terse.
