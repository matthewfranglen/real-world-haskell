module Regexy
    (
        matchesGlob
      , globToRegex
    ) where

import Text.Regex.Posix

matchesGlob :: FilePath -> String -> Either String Bool
matchesGlob pattern name = (name =~) <$> globToRegex pattern

globToRegex :: String -> Either String String
globToRegex cs = ('^':) . (++"$") <$> globToRegex' cs

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
