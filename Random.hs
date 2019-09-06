{-# LANGUAGE TupleSections #-}

module Ren.Random
    ( randomInt
    , randomChoice
    , shuffle
    , randomName
    , roll
    , Dice(..)
    ) where

import System.Random

import Ren.Utils

-- dice notation
-- number of rolls, number of sides, modifier
-- example: 2d4+1 == Dice 2 4 1
data Dice = Dice Int Int Int

-- bounds included
-- work as expected with multiple io uses
randomInt :: Int -> Int -> IO Int
randomInt from = (getStdRandom . randomR) . (from,)

-- actually safe for whatever reason, so you can use it with empty lists
randomChoice :: [a] -> IO a
randomChoice xs =
    fmap (xs !!) (randomInt 0 (length xs - 1))

-- randomly shuffle a list
shuffle :: Eq a => [a] -> IO [a]
shuffle [] = return []
shuffle xs =
    randomChoice xs >>=
        \x -> fmap (x :) (shuffle (remove x xs))

-- roll the dice, minimum result is 1
roll :: Dice -> IO Int
roll (Dice amount sides modifier) =
    let randResult = randomInt 1 sides
    in
        if amount <= 1
        then fmap (\x -> max 1 (x + modifier)) randResult
        else randResult >>= \x ->
            fmap (+x) (roll $ Dice (amount - 1) sides modifier)

-- Random name generation

vowels = "aeiouy"
consonants = "bcdfghjklmnpqrstvwxz"

-- generate a random name
randomName :: IO String
randomName = do
    len <- randomInt 3 11
    coinFlip <- randomInt 0 1
    let isVowelFirst = if coinFlip == 0 then True else False
    buildRandomName isVowelFirst len

buildRandomName :: Bool -> Int -> IO String
buildRandomName _ 0 = return ""
buildRandomName isVowelFirst len =
    let pool = if isVowelFirst then vowels else consonants
    in
        randomChoice pool >>=
            \x -> fmap (x :) (buildRandomName (not isVowelFirst) (len - 1))

