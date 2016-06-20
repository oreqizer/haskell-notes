import Data.Char
import Data.List
-- Functor allow the use of 'fmap'

-- instance Functor IO where
-- fmap f action = do
--     result <- action
--     return (f result)

-- without 'fmap'
-- main :: IO()
-- main = do
--     line <- getLine
--     let line' = reverse line
--     putStrLn $ "You said " ++ line' ++ " backwards!"
--     putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- with 'fmap'
-- main :: IO()
-- main = do
--     line <- fmap reverse getLine
--     putStrLn $ "You said " ++ line ++ " backwards!"
--     putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- 'fmap' function composition
main :: IO()
main = do
    line <- fmap (intersperse '-' . reverse . map toUpper) getLine
    putStrLn line
