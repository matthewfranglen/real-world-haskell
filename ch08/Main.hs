import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Lazy

-- main = isElfFile "./Main.hs" >>= putStrLn . show
-- main = putStrLn . show =<< isElfFile "/bin/bash"
main = putStrLn . show . readPrice =<< BS.readFile "./prices.csv"

isElfFile :: FilePath -> IO Bool
-- isElfFile path = BS.readFile path >>= return . hasElfMagic
-- isElfFile path = fmap hasElfMagic $ BS.readFile path
isElfFile path = hasElfMagic <$> BS.readFile path

hasElfMagic :: BS.ByteString -> Bool
hasElfMagic content = BS.take 4 content == elfMagic
    where elfMagic = Data.ByteString.Lazy.pack [0x7f, 0x45, 0x4c, 0x46]

closingPrice :: BS.ByteString -> Maybe Int
closingPrice = readPrice . (!!4) . BS.split ','

readPrice :: BS.ByteString -> Maybe Int
readPrice str =
    case BS.readInt str of
      Nothing             -> Nothing
      Just (dollars,rest) ->
        case BS.readInt (BS.tail rest) of
          Nothing           -> Nothing
          Just (cents,more) ->
            Just (dollars * 100 + cents)
