module Ren.Json
    ( str
    , arr
    , obj
    ) where

-- Json utils

import Data.List

str :: String -> String
str x = "\"" ++ x ++ "\""

arr :: Show a => [a] -> String
arr xs = "[" ++ content ++ "]"
    where
        content = intercalate "," (map show xs)

obj :: [(String, String)] -> String
obj xs = "{" ++ content ++ "}"
    where
        content = intercalate "," (map param xs)

param :: (String, String) -> String
param (key, val) = (str key) ++ ":" ++ val
