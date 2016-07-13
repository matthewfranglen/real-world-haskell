module Regexy
    (
        fileNameMatches
      , globToRegex
    ) where

import Text.Regex.Posix

fileNameMatches :: String -> String -> Maybe Bool
fileNameMatches pattern name = (name =~) <$> globToRegex pattern

globToRegex :: String -> Maybe String
globToRegex cs = ('^':) . (++"$") <$> globToRegex' cs

globToRegex' :: String -> Maybe String
globToRegex' "" = Just ""

globToRegex' ('*':cs) = (".*"++) <$> globToRegex' cs

globToRegex' ('?':cs) = ('.':) <$> globToRegex' cs

globToRegex' ('[':'!':c:cs) = ("[^"++) . (c:) <$> charClass cs
globToRegex' ('[':c:cs)     = ('[':) . (c:) <$> charClass cs
globToRegex' ('[':_)        = Nothing -- unterminated character class

globToRegex' (c:cs) = ((escape c)++) <$> globToRegex' cs

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Maybe String
charClass (']':cs) = (']':) <$> globToRegex' cs
charClass (c:cs)   = (c:) <$> charClass cs
charClass []       = Nothing -- unterminated character class
