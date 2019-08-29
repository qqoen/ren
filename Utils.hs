module Ren.Utils
    ( tryIdx
    , tryRead
    , mapIndexes
    , mapi
    , remove
    , quote
    ) where

-- safe (!!)
tryIdx :: Int -> [a] -> Maybe a
tryIdx i xs =
    if i >= length xs
        then Nothing
        else Just (xs !! i)

-- safe read
tryRead :: Read a => String -> Maybe a
tryRead s =
    case reads s of
        [(val, "")] -> Just val
        _           -> Nothing

-- get list of elements+indexes
mapIndexes :: [a] -> Int -> [(a, Int)]
mapIndexes [] _ = []
mapIndexes (x:xs) i = (x, i) : (mapIndexes xs (i + 1))

-- map list with element index
mapi :: (a -> Int -> b) -> [a] -> [b]
mapi fn xs =
    map (\(x, i) -> fn x i) (mapIndexes xs 0)

-- remove elem from  a list
remove :: Eq a => a -> [a] -> [a]
remove x xs = filter (x /=) xs

quote :: String -> String
quote str = "\"" ++ str ++ "\""
