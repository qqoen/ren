module Ren.Data.Count
    ( count
    , minCount
    , maxCount
    , countE
    , cCur
    , cMin
    , cMax
    , apply
    , apply2
    , applyE
    , isMax
    , isMin
    , inRange
    , Count
    ) where

-- current, min, max
data Count = Count Int Int Int

instance Show Count where
    show count = (show $ cCur count) ++ "/" ++ (show $ cMax count)

-- Getters

cCur (Count cur _ _) = cur
cMin (Count _ min _) = min
cMax (Count _ _ max) = max

-- Constructors

count :: Int -> Int -> Int -> Count
count initCur minv maxv =
    trunc (Count initCur minv maxv)

minCount :: Int -> Int -> Count
minCount minv maxv =
    trunc (Count minv minv maxv)

maxCount :: Int -> Int -> Count
maxCount minv maxv =
    trunc (Count maxv minv maxv)

countE :: Int -> Int -> Int -> Either Count Count
countE curv minv maxv =
    let newCount = Count curv minv maxv
    in getEither newCount

-- Operations

apply :: (Int -> Int) -> Count -> Count
apply f count =
    let
        minv = cMin count
        maxv = cMax count
        newCur = f (cCur count)
    in trunc (Count newCur minv maxv)

apply2 :: (Int -> Int -> Int) -> Count -> Count -> Count
apply2 f count1 count2 =
    let newCur = f (cCur count1) (cCur count2)
    in trunc (Count newCur (cMin count1) (cMax count2))

applyE :: (Int -> Int) -> Count -> Either Count Count
applyE f count =
    let newCount = apply f count
    in getEither newCount

-- Predicates

isMax :: Count -> Bool
isMax count = cCur count == cMax count

isMin :: Count -> Bool
isMin count = cCur count == cMin count

inRange :: Count -> Bool
inRange count = cCur count >= cMin count && cCur count <= cMax count

-- Helpers

trunc :: Count -> Count
trunc count =
    let
        minv = cMin count
        maxv = cMax count
        val = max minv (min maxv (cCur count))
    in Count val minv maxv

getEither :: Count -> Either Count Count
getEither count =
    if inRange count
        then Right count
        else Left count
