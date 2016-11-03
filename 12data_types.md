# Data types

An *algebraic data type* is a type created by combining other types. The most common example is a **list**, which is defined as:

```Haskell
data List a = Nil | Cons a (List a)
```

That basically means `Cons` (which stands for *construct*) holds an element of type `a` and is followed by a list of the type `a`.

> We have a special syntax for `Cons` using `:`, and `[]` for `Nil`. There's also a syntactic sugar for whole lists by simply wrapping all elements in `[]`, which is the most common way of writing them. The following are all equivalent:
>
> * `Cons 1 (Cons 2 (Cons 3 Nil))`
> * `1:2:3:[]`
> * `[1,2,3]`

There are three types:

* `data` creates a new type
* `type` creates a type alias
* `newtype` creates a *strictly evaluated* single-constructor single-parameter type

### Data

As seen above, types are defined using the `data` keyword:

```Haskell
data Bool = False | True
```

Here, `data` says we're creating a new type (`Bool` in this case). Things after `=` separated by `|` are **value constructors**.

Another example:

```Haskell
data Shape = Circle Float Float Float
           | Rectangle Float Float Float Float
```

`Circle` takes three floats to be constructed, `Rectangle` takes four. We could simplify this by defining a `Point` type, as both of the shapes use some:

```Haskell
data Point = Point Float Float

data Shape = Circle Point Float  -- Float is the radius
           | Rectangle Point Point
```

#### Terms & Arity

Three names to remember:

* **Concrete type** is a type of which we can create values (`Int`, `Double`...)
* **Value constructor** creates a new *value* of a *type* when supplied with values
* **Type constructor** creates a new *type* by being supplied another type

Both *value constructors* and *type constructors* can be have different **arity** depending on how many arguments they take.

Nullary type constructors are concrete types - we can make value off of them:

```Haskell
data Point = Point Float Float

zero :: Point
zero = Point 0 0
```

> Type constructor and value constructors don't have to be named the same. It's just a common practice to name them equally when a type has only one value constructor.

Here, `Point` is a concrete type, and `Point Float Float` is the value constructor.

#### Type parameters

We create a *concrete type* by supplying the *type constructor* with a **type parameter**:

```Haskell
data Point a = Point a a

zero :: Point Int  -- expects a value of Point Int Int
zero = Point 0 0

leet :: Point Float  -- expects a value of Point Float Float
leet = Point 13.37 13.37
```

Type constructors can have an arity of *1-n*.

```Haskell
data Point a = Point a a

data Circle p r = Circle p r  -- p - point, r - radius

unit :: Circle (Point Int) Float
unit = Circle (Point 0 0) 1.0
```

### Type

Defined with the `type` keyword. It's basically an alias for another type. They are the same in every sense, the name is only used for clarifying what the underlying type is supposed to *mean*. The most common one is `String`, which is basically:

```Haskell
type String = [Char]
```

We can type check `[Char]` with `String` and vice versa. The following are equivalent:

```Haskell
hello :: [Char] -> [Char]
hello = (++) "Hello "

hello' :: String -> String
hello' = (++) "Hello "
```

Taking this to our example with shapes, we can define `Circle` as such (for convenience, we'll separate `Circle` and `Rectangle` to separate types):

```Haskell
type Radius = Float

data Point = Point Float Float

data Circle = Circle Point Radius
```

### Newtype

Like `data` with **one** constructor that has **one** field. There are differences that justify the actual existence of `newtype`:

* constructors are erased at *compile time*, thus no need for unboxing
* guarantees the exact same representation as the wrapped type
* has different *strictness* properties than `data`

Only types with a *single constructor* can be wrapped in a `newtype` - it is thus not algebraic. Also, when using the *record syntax*, only one field can be declared.

One of the common uses of `newtype` is to give additional properties to an existing type, for example custom implementation of a type class.

### Deriving

When creating a new `data` or `newtype`, we can *derive* instances for our type:

```Haskell
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday
    deriving (Eq, Ord, Bounded, Enum, Show, Read)
```

Now we can:

* compare equality via `Eq`
* order the days via `Ord`
* check lowest or highest value via `Bounded`
* `pred` or `succ` it via `Enum`
* print it to string via `Show`
* read it from user input via `Read`

All this functionality for free! There are definitions for `deriving` each typeclass in the *Haskell 98* spec, but it's magic on the surface.
