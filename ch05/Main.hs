module Main (main) where

import SimpleJSON (JValue(..))
import Serializer (serialize)
import Deserializer (deserialize)

main = do
    let object = (JObject [("foo", JNumber 1), ("bar", JBool False), ("quxx", JArray [JString "towel", JNull])])
    putStrLn $ show object
    putStrLn $ serialize object
    putStrLn $ show $ deserialize $ serialize object
