module Person where

    data Person = Person {
        firstName :: String,
        lastName :: String,
        age :: Int,
        height :: Float,
        phoneNumber :: String,
        flavor :: String
    } deriving (Eq, Read, Show)

    data Car = Car {
        company :: String,
        model :: String,
        year :: Int
    } deriving (Show)

    data Day
        = Monday
        | Tuesday
        | Wednesday
        | Thursday
        | Friday
        | Saturday
        | Sunday
        deriving (Eq, Ord, Show, Read, Bounded, Enum) 
