module Regexy
    (
        matchesGlob
      , globToRegex
    ) where

import Text.Regex.PCRE
import Data.Bits ((.|.))

matchesGlob :: Bool -> FilePath -> String -> Either String Bool
matchesGlob cs pattern name = (`matchTest` name) <$> globToRegex cs pattern

globToRegex :: Bool -> String -> Either String Regex
globToRegex cs xs = f . ('^':) . (++"$") <$> globToRegex' xs
    where f | cs        = makeRegexOpts caseSensitive defaultExecOpt
            | otherwise = makeRegexOpts caseInsensitive defaultExecOpt

caseSensitive = defaultCompOpt
caseInsensitive = defaultCompOpt .|. compCaseless

globToRegex' :: String -> Either String String
globToRegex' "" = Right ""

globToRegex' ('*':cs) = (".*"++) <$> globToRegex' cs

globToRegex' ('?':cs) = ('.':) <$> globToRegex' cs

globToRegex' ('[':'!':c:cs) = ("[^"++) . (c:) <$> charClass cs
globToRegex' ('[':c:cs)     = ('[':) . (c:) <$> charClass cs
globToRegex' ('[':_)        = Left "unterminated character class"

globToRegex' (c:cs) = ((escape c)++) <$> globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Either String String
charClass (']':cs) = (']':) <$> globToRegex' cs
charClass (c:cs)   = (c:) <$> charClass cs
charClass []       = Left "unterminated character class"
