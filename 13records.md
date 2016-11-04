# Records

Record syntax is used for, well, records:

```Haskell
data Person = Person { name :: String
                     , age  :: Int
                     , food :: String
                     } deriving (Eq, Show)
```

This creates the functions `name`, `age` and `food`, that will return the values of the fields of the specific record.

### Ordered fields

We can use records with specifying fields by their order:

```Haskell
getDohn :: Person
getDohn = Person "Dohn" 32 "Pancakes"
```

### Named fields

Creating, updating and using the record using named fields:

```Haskell
getJohn :: Person
getJohn = Person {name="John", age=28, food="Lasagne"}

getOldJohn :: Person
getOldJohn = getJohn {age=75}

-- using the supplied functions
description :: Person -> String
description p =
    name p ++ ", aged " ++ show (age p) ++ ", loves " ++ food p

-- pattern matching
description' :: Person -> String
description' Person {name=n, age=a, food=f} =
    n ++ ", aged " ++ show a ++ ", loves " ++ f
```

We can also *patten match* and *capture* certain values of the record:

```Haskell
greetJohn :: Person -> String
greetJohn Person {name=n@"John"} = "Hello, " ++ n  -- the 'n@' is optional
greetJohn _ = "You ain't John!"
```
