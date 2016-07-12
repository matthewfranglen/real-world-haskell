import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (chr)

-- main = isElfFile "./Main.hs" >>= putStrLn . show
-- main = putStrLn . show =<< isElfFile "/bin/bash"
main =
    BS.readFile "./prices.csv" >>=
    return . (!!1) . BS.lines  >>=
    putStrLn . show . closingPrice

isElfFile :: FilePath -> IO Bool
-- isElfFile path = BS.readFile path >>= return . hasElfMagic
-- isElfFile path = fmap hasElfMagic $ BS.readFile path
isElfFile path = hasElfMagic <$> BS.readFile path

hasElfMagic :: BS.ByteString -> Bool
hasElfMagic content = BS.take 4 content == elfMagic
    where elfMagic = BS.pack $ map chr [0x7f, 0x45, 0x4c, 0x46]

closingPrice :: BS.ByteString -> Maybe Int
closingPrice = readPrice . (!!4) . BS.split ','

readPrice :: BS.ByteString -> Maybe Int
readPrice str = BS.readInt str >>= readFraction
    where readFraction :: (Int, BS.ByteString) -> Maybe Int
          readFraction (dollars, rest) = (+dollarsInCents) <$> fst <$> BS.readInt (BS.tail rest)
              where dollarsInCents = dollars * 100

-- readPrice str = BS.readInt str >>= readFraction
--     where readFraction :: (Int, BS.ByteString) -> Maybe Int
--           readFraction (dollars, rest) = BS.readInt rest >>= Just . (+dollarsInCents) . fst
--               where dollarsInCents = dollars * 100

-- readPrice :: BS.ByteString -> Maybe Int
-- readPrice str =
--     case BS.readInt str of
--       Nothing             -> Nothing
--       Just (dollars,rest) ->
--         case BS.readInt (BS.tail rest) of
--           Nothing           -> Nothing
--           Just (cents,more) ->
--             Just (dollars * 100 + cents)
