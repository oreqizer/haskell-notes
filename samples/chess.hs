module Main where

import Text.Printf

chess :: Int -> String
chess x = chessCol x x
    where chessCol _ 0 = ""
          chessCol y n = chessRow y n ++ "\n" ++ chessCol y (n - 1)

chessRow :: Int -> Int -> String
chessRow x y
    | x <= 0 = ""
    | (x + y) `mod` 2 == 0 = "x " ++ chessRow (x - 1) y
    | otherwise = "o " ++ chessRow (x - 1) y

main :: IO ()
main = do
    print "Side:"
    x <- getLine
    printf (chess $ read x)
