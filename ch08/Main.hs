import qualified Data.ByteString.Lazy as BS

-- main = isElfFile "./Main.hs" >>= putStrLn . show
main = isElfFile "/bin/bash" >>= putStrLn . show

isElfFile :: FilePath -> IO Bool
-- isElfFile path = BS.readFile path >>= return . hasElfMagic
-- isElfFile path = fmap hasElfMagic $ BS.readFile path
isElfFile path = hasElfMagic <$> BS.readFile path

hasElfMagic :: BS.ByteString -> Bool
hasElfMagic content = BS.take 4 content == elfMagic
    where elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]
