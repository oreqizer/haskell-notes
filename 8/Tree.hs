module Tree where

    -- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

    infixr 5 :-:
    data List a = Empty | a :-: (List a) deriving (Show, Read, Eq, Ord)

    infixr 5  .++
    (.++) :: List a -> List a -> List a
    Empty .++ ys = ys
    (x :-: xs) .++ ys = x :-: (xs .++ ys)

    -- Trees
    -- =====
    data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

    singleton :: a -> Tree a
    singleton x = Node x EmptyTree EmptyTree

    treeError :: error
    treeError = error "You entered bullshit"

    treeInsert :: (Ord a) => a -> Tree a -> Tree a
    treeInsert x EmptyTree = singleton x
    treeInsert x (Node a left right)
        | x == a = Node x left right
        | x < a  = Node a (treeInsert x left) right
        | x > a  = Node a left (treeInsert x right)
        | otherwise = treeError

    treeElem :: (Ord a) => a -> Tree a -> Bool
    treeElem _ EmptyTree = False
    treeElem x (Node a left right)
        | x == a = True
        | x < a  = treeElem x left
        | x > a  = treeElem x right
        | otherwise = treeError

    instance Functor Tree where
        fmap _ EmptyTree = EmptyTree
        fmap f (Node x leftsub rightsub) =
            Node (f x) (fmap f leftsub) (fmap f rightsub)
