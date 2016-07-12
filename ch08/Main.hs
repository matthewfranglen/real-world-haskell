import qualified Data.ByteString.Lazy as BS

isElfFile :: FilePath -> IO Bool
isElfFile path = BS.readFile path >>= return . hasElfMagic

hasElfMagic :: BS.ByteString -> Bool
hasElfMagic content = BS.take 4 content == elfMagic
    where elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]
