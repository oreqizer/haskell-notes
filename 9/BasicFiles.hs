import System.IO

-- type FilePath = String

-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

-- brute force file opening
-- main :: IO ()
-- main = do
--     handle <- openFile "song.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- withFile syntactic sugar
-- main :: IO ()
-- main = withFile "song.txt" ReadMode (\handle -> do
--         contents <- hGetContents handle
--         putStr contents)

-- readFile syntactic sugar
main :: IO ()
main = do
    contents <- readFile "song.txt"
    putStr contents

-- more file functions:
-- writeFile
-- appendFile

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
    handle <- openFile path mode
    result <- f handle
    hClose handle
    return result

-- BlockBuffering:
-- main = do
--     withFile "something.txt" ReadMode (\handle -> do
--         hSetBuffering handle $ BlockBuffering (Just 2048)
--         contents <- hGetContents handle
--         putStr contents)
