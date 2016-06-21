import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch =  [("add", add),
             ("view", view),
             ("remove", remove)]

main :: IO ()
main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")
add _ = putStr "Bad input"

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith
            (\n line -> show n ++ " - " ++ line) [0 :: Integer, 1..] todoTasks
    putStr $ unlines numberedTasks
view _ = putStr "Bad input"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString
        todoTasks = lines contents
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName
remove _ = putStr "Bad input"

-- import System.IO
-- import System.Directory
-- import Data.List
--
-- main :: IO ()
-- main = do
--     handle <- openFile "todo.txt" ReadMode
--     (tempName, tempHandle) <- openTempFile "." "temp"
--     contents <- hGetContents handle
--     let todoTasks = lines contents
--         numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
--     putStrLn "These are your TO-DO items:"
--     putStr $ unlines numberedTasks
--     putStrLn "Which one do you want to delete?"
--     numberString <- getLine
--     let number = read numberString
--         newTodoItems = delete (todoTasks !! number) todoTasks
--     hPutStr tempHandle $ unlines newTodoItems
--     hClose handle
--     hClose tempHandle
--     removeFile "todo.txt"
--     renameFile tempName "todo.txt"
