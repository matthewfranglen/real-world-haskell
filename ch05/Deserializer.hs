module Deserializer
    (
        deserialize
    ) where

import Data.Char (isSpace, isNumber)

import SimpleJSON (JValue(..))

deserialize :: String -> JValue
deserialize = parse . tokenize

parse :: [Token] -> JValue
parse []            = error "No token"
parse [(TString s)] = JString s
parse [(TNumber n)] = undefined

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
tokenize []                       = []
tokenize ('[':xs)                 = TOpenArray : tokenize xs
tokenize (']':xs)                 = TCloseArray : tokenize xs
tokenize ('{':xs)                 = TOpenObject : tokenize xs
tokenize ('}':xs)                 = TCloseObject : tokenize xs
tokenize (':':xs)                 = TKeyValueSeparator : tokenize xs
tokenize (',':xs)                 = TElementSeparator : tokenize xs
tokenize ('"':xs)                 = tokenizeString "" xs
tokenize ('t':'r':'u':'e':xs)     = TBool True : tokenize xs
tokenize ('f':'a':'l':'s':'e':xs) = TBool False : tokenize xs
tokenize ('n':'u':'l':'l':xs)     = TNull : tokenize xs
tokenize (x:xs)                   | isSpace x              = tokenize xs
                                  | isNumber x || x == '-' = tokenizeNumber [] (x:xs)

tokenizeString :: String -> String -> [Token]
tokenizeString _ []              = error "Parse error: Premature end inside string"
tokenizeString xs' ('"':xs)      = TString (decode xs') : tokenize xs
tokenizeString xs' ('\\':'"':xs) = tokenizeString (xs' ++ "\"") xs
tokenizeString xs' (x:xs)        = tokenizeString (xs' ++ [x]) xs

tokenizeNumber :: String -> String -> [Token]
tokenizeNumber xs' []                              = [TNumber xs']
tokenizeNumber xs' (x:xs) | isNumber x || x == '-' = tokenizeNumber (xs' ++ [x]) xs
                          | x == '.'               = tokenizeFractionalNumber (xs' ++ [x]) xs
                          | otherwise              = TNumber xs' : tokenize (x:xs)

tokenizeFractionalNumber xs' []                  = [TNumber xs']
tokenizeFractionalNumber xs' (x:xs) | isNumber x = tokenizeFractionalNumber (xs' ++ [x]) xs
                                    | otherwise  = TNumber xs' : tokenize (x:xs)

decode :: String -> String
decode xs = xs
