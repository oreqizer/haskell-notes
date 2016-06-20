module Classes where

    data TrafficLight = Red | Yellow | Green

    instance Eq TrafficLight where
        Red == Red = True
        Green == Green = True
        Yellow == Yellow = True
        _ == _ = False

    instance Show TrafficLight where
        show Red = "Red light"
        show Yellow = "Yellow light"
        show Green = "Green light"

    -- instance (Eq m) => Eq (Maybe m) where
    --     Just x == Just y = x == y
    --     Nothing == Nothing = True
    --     _ == _ = False

    -- instance Functor (Either a) where
    --     fmap f (Right x) = Right (f x)
    --     fmap f (Left x) = Left x
