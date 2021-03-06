module Closy
    (
        closingPrice,
        highestClose
    ) where

import qualified Data.ByteString.Lazy.Char8 as BS

highestClose :: BS.ByteString -> Maybe Int
highestClose = maximum . (Nothing :) . map closingPrice . BS.lines

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
