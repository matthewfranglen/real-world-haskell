module Serializer
    (
        serialize
    ) where

import SimpleJSON (JValue(..))
import Render (renderBool, renderNull, renderNumber, renderText)

serialize :: JValue -> String
serialize = stringify . flatten

stringify :: [Token] -> String
stringify ts = s 0 ts
    where s _ []               = []
          s i ((TString t):ts) = t ++ s i ts
          s i (TIndent:ts)     = s (i + 1) ts
          s i (TUnindent:ts)   = s (i - 1) ts
          s i (TBreak:ts)      = '\n' : (replicate indent ' ') ++ s i ts
            where indent = i * 4

flatten :: JValue -> [Token]
flatten JNull       = [TString renderNull]
flatten (JString s) = [TString $ renderText s]
flatten (JNumber n) = [TString $ renderNumber n]
flatten (JBool b)   = [TString $ renderBool b]
flatten (JArray as) = start ++ flat ++ end
    where start = [open, TIndent, TBreak]
          flat  = series $ map flatten as
          end   = [TUnindent, TBreak, close]
          open  = TString "["
          close = TString "]"
flatten (JObject os) = start ++ flat ++ end
    where start        = [open, TIndent, TBreak]
          flat         = series $ map oflat os
          oflat (s, v) = (TString $ renderText s) : sep : flatten v
          end          = [TUnindent, TBreak, close]
          sep          = TString ": "
          open         = TString "{"
          close        = TString "}"

data Token = TString String
           | TIndent
           | TUnindent
           | TBreak
             deriving (Show)

series :: [[Token]] -> [Token]
series []     = []
series [x]    = x
series (x:xs) = x ++ (comma : TBreak : series xs)
    where comma = TString ","

join :: Char -> [String] -> String
join sep []     = []
join sep [x]    = x
join sep (x:xs) = x ++ (sep : join sep xs)
