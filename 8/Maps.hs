module Phonebook where

    import qualified Data.Map as Map

    type Name = String
    type Number = String
    type PhoneBook = [(Name, Number)]

    phoneBook :: PhoneBook
    phoneBook = [
            ("betty", "555-2938"),
            ("bonnie", "452-2928"),
            ("patsy", "493-2928"),
            ("lucille", "205-2928"),
            ("wendy", "939-8282"),
            ("penny", "853-2492")
        ]

    inPhoneBook :: Name -> Number -> PhoneBook -> Bool
    inPhoneBook name number pbook = (name, number) `elem` pbook

    type AssocList k v = [(k,v)]

    getKey :: (Eq k) => k -> AssocList k v -> Maybe v
    getKey k = foldr reduce Nothing
        where reduce (key, val) acc = if key == k then Just val else acc

    type IntMap = Map.Map Int
    -- type IntMap v = Map.Map Int v

    data LockerState = Taken | Free deriving (Show, Eq)

    type Code = String

    type LockerMap = Map.Map Int (LockerState, Code)

    lockerLookup :: Int -> LockerMap -> Either String Code
    lockerLookup lockerNumber list =
        case Map.lookup lockerNumber list of
            Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
            Just (state, code) -> if state /= Taken
                then Right code
                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

    lockers :: LockerMap
    lockers = Map.fromList [
            (100, (Taken,"ZD39I")),
            (101, (Free,"JAH3I")),
            (103, (Free,"IQSA9")),
            (105, (Free,"QOTSA")),
            (109, (Taken,"893JJ")),
            (110, (Taken,"99292"))
        ]

    -- instance (Ord k) => Functor (Map.Map k) where
    --     fmap f [] = []
    --     fmap f ((k, v):xs) = (k, f v) : fmap xs
