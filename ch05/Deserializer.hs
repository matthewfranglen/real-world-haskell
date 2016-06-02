module Deserializer
    (
        deserialize
    ) where

import Data.Char (isSpace, isNumber)
import Text.Read (readMaybe)

import SimpleJSON (JValue(..))

deserialize :: String -> JValue
deserialize = parse . tokenize

parse :: [Token] -> JValue
parse xs | null xs'  = o
         | otherwise = error "Tokens continue past end of object"
    where (o, xs') = parseValue xs

parseValue :: [Token] -> (JValue, [Token])
parseValue []               = error "Parse error: No token"
parseValue ((TString s):xs) = (JString s, xs)
parseValue ((TNumber n):xs) = case readMaybe n :: Maybe Double of
    Just d -> (JNumber d, xs)
    _      -> error "Parse error: Unparseable number"
parseValue ((TBool x):xs) = (JBool x, xs)
parseValue (TNull:xs)     = (JNull, xs)

parseValue (TOpenArray:xs) = parseArray [] xs

parseValue (TOpenObject:xs) = parseObject [] xs

parseArray :: [JValue] -> [Token] -> (JValue, [Token])
parseArray xs' []               = error "Parse error: Premature end inside string"
parseArray xs' (TCloseArray:xs) = (JArray xs', xs)
parseArray xs' xs | g == TElementSeparator = parseArray xs'' gs
                  | g == TCloseArray       = ((JArray xs''), gs)
                  | otherwise              = error "Parse error: No separator inside array"
    where (x, (g:gs)) = parseValue xs
          xs''        = xs' ++ [x]

parseObject :: [(String, JValue)] -> [Token] -> (JValue, [Token])
parseObject xs' [] = error "Parse error: Premature end inside object"
parseObject xs' (TCloseObject:xs) = (JObject xs', xs)
parseObject xs' ((TString k):TKeyValueSeparator:xs) | g == TElementSeparator = parseObject xs'' gs
                                                    | g == TCloseObject      = ((JObject xs''), gs)
                                                    | otherwise              = error "Parse error: No separator inside object"
    where (x, (g:gs)) = parseValue xs
          xs''        = xs' ++ [(k, x)]

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
             deriving (Show, Eq)

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
