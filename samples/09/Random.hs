module Random where

    import System.Random
    import Data.List()

    threeCoins :: StdGen -> (Bool, Bool, Bool)
    threeCoins gen =
        let (firstCoin, newGen) = random gen
            (secondCoin, newGen') = random newGen
            (thirdCoin, _) = random newGen'
        in  (firstCoin, secondCoin, thirdCoin)

    randoms' :: (RandomGen g, Random a) => g -> [a]
    randoms' gen = let (value, newGen) = random gen in value:randoms' newGen

    finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
    finiteRandoms 0 gen = ([], gen)
    finiteRandoms n gen =
        let (value, newGen) = random gen
            (restOfList, finalGen) = finiteRandoms (n-1) newGen
        in  (value:restOfList, finalGen)

    -- main :: IO ()
    -- main = do
    --     gen <- getStdGen
    --     let randomChars = randomRs ('a','z') gen
    --         (first20, rest) = splitAt 20 randomChars
    --         (second20, _) = splitAt 20 rest
    --     putStrLn first20
    --     putStr second20

    main :: IO ()
    main = do
        gen <- getStdGen
        putStrLn $ take 20 (randomRs ('a','z') gen)
        gen' <- newStdGen
        putStr $ take 20 (randomRs ('a','z') gen')
