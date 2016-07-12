module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Lib

-- main = isElfFile "./Main.hs" >>= putStrLn . show
-- main = putStrLn . show =<< isElfFile "/bin/bash"
main =
    BS.readFile "./prices.csv" >>=
    putStrLn . show . highestClose
