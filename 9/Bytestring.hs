module Bytestrings where
    import qualified Data.ByteString.Lazy as B
    import qualified Data.ByteString as S
    import System.Environment

    pack :: String
    pack = show $ B.pack [98..120]

    unpack :: String
    unpack = show $ B.unpack $ B.pack [98..120]

    fromChunks :: String
    fromChunks = show $
        B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

    toChunks :: String
    toChunks = show $ B.toChunks $
        B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]

    -- lazy:
    cons :: String
    cons = show $ foldr B.cons B.empty [50..60]

    -- not lazy:
    cons' :: String
    cons' = show $ foldr B.cons' B.empty [50..60]

    main :: IO ()
    main = do
        (fileName1:fileName2:_) <- getArgs
        copyFile fileName1 fileName2

    copyFile :: FilePath -> FilePath -> IO ()
    copyFile source dest = do
        contents <- B.readFile source
        B.writeFile dest contents
