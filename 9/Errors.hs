import System.Environment
import System.IO()
import Control.Exception
-- import System.IO.Error
-- import System.Directory

-- main :: IO ()
-- main = do (fileName:_) <- getArgs
--           contents <- readFile fileName
--           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

-- main :: IO ()
-- main = do
--         (fileName:_) <- getArgs
--         fileExists <- doesFileExist fileName
--         if fileExists
--             then do contents <- readFile fileName
--                     putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
--             else putStrLn "The file doesn't exist!"

main :: IO ()
main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler = print
