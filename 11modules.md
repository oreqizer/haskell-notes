# Modules

A collection of *functions*, *types* and *typeclasses*.

### Import

Syntax: `import <module name>`

Simply importing a *module* makes every exported function accessible in the *global namespace*:

```Haskell
import Data.List
```

Import only `nub` and `sort`:

```Haskell
import Data.List (nub, sort)
```

Import all except `nub`:

```Haskell
import Data.List hiding (nub)
```

**Qualified:**

Qualified imports don't make stuff available in the *global namespace* - we have to refer to them via the module name - `Data.List.nub` for example.

```Haskell
import qualified Data.Map
```

Renaming the import:

```Haskell
import qualified Data.Map as M
```

### Export

Say we have a file `Geometry.hs` with some functions. This is how we *export* them:

```Haskell
module Geometry
( sphereVolume
, sphereArea
, cubeVolume
, cubeArea
, cuboidArea
, cuboidVolume
) where

sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  

sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  

cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  

cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  

cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  

cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  

-- note: not exported
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b
```

To use this fancy-dancy geometry module, we simply do:

```Haskell
import Geometry
```

and maybe with one of the modified we import stuff.

### Hierarchical modules

To create a more granular version of the `Geometry` module, create a folder called `Geometry`, then put three files there - `Sphere.hs`, `Cuboid.hs`, and `Cube.hs`.

**Sphere.hs**

```Haskell
module Geometry.Sphere
( volume
, area
) where

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

area :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
```

**Cuboid.hs**

```Haskell
module Geometry.Cuboid
( volume
, area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = rectangleArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2

rectangleArea :: Float -> Float -> Float
rectangleArea a b = a * b
```

**Cube.hs**

```Haskell
module Geometry.Cube
( volume
, area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume side = Cuboid.volume side side side

area :: Float -> Float
area side = Cuboid.area side side side
```

We import them as expected:

```Haskell
import Geometry.Sphere
```

Then `Geometry.Sphere`'s functions will be available. If we wanted to import more modules with conflicting function names, *qualified import* is our friend:

```Haskell
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid
import qualified Geometry.Cube as Cube
```
