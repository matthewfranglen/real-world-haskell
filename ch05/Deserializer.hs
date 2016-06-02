module Deserializer
    (
        deserialize
    ) where

import Data.Char (isSpace, isNumber)

import SimpleJSON (JValue(..))

deserialize :: String -> JValue
deserialize = parse . tokenize

parse :: [Token] -> JValue
parse xs = undefined

data Token = TOpenArray
           | TCloseArray
           | TOpenObject
           | TCloseObject
           | TKeyValueSeparator
           | TElementSeparator
           | TString String
           | TNumber String
           | TBool Bool
           | TNull
             deriving (Show)

tokenize :: String -> [Token]
tokenize []        = []
tokenize ('[':xs)  = TOpenArray : tokenize xs
tokenize (']':xs)  = TCloseArray : tokenize xs
tokenize ('{':xs)  = TOpenObject : tokenize xs
tokenize ('}':xs)  = TCloseObject : tokenize xs
tokenize (':':xs)  = TKeyValueSeparator : tokenize xs
tokenize (',':xs)  = TElementSeparator : tokenize xs
tokenize ('"':xs)  = s "" xs
    where s _ []              = error "Parse error: Premature end inside string"
          s xs' ('"':xs)      = TString (decode xs') : tokenize xs
          s xs' ('\\':'"':xs) = s (xs' ++ "\"") xs
          s xs' (x:xs)        = s (xs' ++ [x]) xs

tokenize (x:xs) | isSpace x              = tokenize xs
                | isNumber x || x == '-' = n [] (x:xs)
    where n xs' []                              = [TString xs']
          n xs' (x:xs) | isNumber x || x == '-' = n (xs' ++ [x]) xs
                       | x == '.'               = fn (xs' ++ [x]) xs
                       | otherwise              = TString xs' : tokenize (x:xs)
          fn xs' []                  = [TString xs']
          fn xs' (x:xs) | isNumber x = fn (xs' ++ [x]) xs
                        | otherwise  = TString xs' : tokenize (x:xs)

decode :: String -> String
decode xs = xs
