module Hello where

    import System.IO()
    import Control.Monad

    -- main = do   
    -- line <- getLine
    -- if null line
    --     then return ()
    --     else do
    --         putStrLn $ reverseWords line
    --         main

    main :: IO ()
    main = do
        line <- getLine
        unless (null line) $ do
            putStrLn $ reverseWords line
            main

    reverseWords :: String -> String
    reverseWords = unwords . map reverse . words
