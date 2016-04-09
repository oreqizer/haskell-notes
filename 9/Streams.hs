-- import Control.Monad
-- import Data.Char

-- main :: IO ()
-- main = forever $ do
--     putStr "Give me some input: "
--     l <- getLine
--     putStrLn $ map toUpper l

-- main :: IO ()
-- main = do
--     contents <- getContents
--     putStr (map toUpper contents)

main :: IO ()
main = do
    contents <- getContents
    putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
    let allLines = lines input
        shortLines = filter (\line -> length line < 10) allLines
        result = unlines shortLines
    in  result

-- main = interact $ unlines . filter ((<10) . length) . lines  
