module Colors where

    import Control.Monad

    forIntM :: (Monad m) => [Int] -> (Int -> m b) -> m [b]
    forIntM = forM

    main :: IO [()]
    main = do
        colors <- forIntM [1,2,3,4] (\a -> do
            putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
            -- color <- getLine
            -- return color
            getLine)
        putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
        mapM putStrLn colors
