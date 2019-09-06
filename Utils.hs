module Ren.Utils
    ( tryIdx
    , tryRead
    , mapIndexes
    , mapi
    , remove
    ) where

import Data.Tuple (uncurry)

-- safe (!!)
tryIdx :: Int -> [a] -> Maybe a
tryIdx i xs
    | i >= length xs = Nothing
    | otherwise      = Just (xs !! i)

-- safe read
tryRead :: Read a => String -> Maybe a
tryRead s =
    case reads s of
        [(val, "")] -> Just val
        _           -> Nothing

-- assign and index to each item of a list, starting with provided value
mapIndexes :: Int -> [a] -> [(a, Int)]
mapIndexes _ []     = []
mapIndexes i (x:xs) = (x, i) : (mapIndexes (i + 1) xs)

-- map list with element index
mapi :: (a -> Int -> b) -> [a] -> [b]
mapi fn = map (uncurry fn) . (mapIndexes 0)

-- remove elem from  a list
remove :: Eq a => a -> [a] -> [a]
remove = filter . (/=)
