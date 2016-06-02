module Render
    (
        renderBool,
        renderNull,
        renderNumber,
        renderText
    ) where

import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.))

renderText :: String -> String
renderText s = foldr (:) [quote] (quote : escapedChars)
    where quote        = '"'
          escapedChars = foldr (++) [] (map escape s)

renderNumber :: (Num a, Show a) => a -> String
renderNumber = show

renderBool :: Bool -> String
renderBool True  = "true"
renderBool False = "false"

renderNull :: String
renderNull = "null"

escape :: Char -> String
escape c = case lookup c simpleEscapes of
              Just r -> r
              Nothing | mustEscape c -> hexEscape c
                      | otherwise    -> [c]
    where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b = (a, ['\\',b])

smallHex :: Int -> String
smallHex x  = "\\u"
           ++ replicate (4 - length h) '0'
           ++ h
    where h = showHex x ""

astral :: Int -> String
astral n = smallHex (a + 0xd800) ++ smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

hexEscape :: Char -> String
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c
