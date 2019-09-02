-- Console UI utilities

module Ren.Console.UI
    ( runMenu
    , printBar
    , printDivider
    , printDividerOf
    , Menu
    , MenuItem
    , MenuOptions(..)
    ) where

import System.IO
import Data.Foldable (fold)

import System.Console.ANSI
import System.Console.Terminal.Size

import Ren.Utils as U
import Ren.Console.Base

-- public api

-- name, description, is available, return value
type MenuItem a = (String, String, Bool, a)
type Menu a = [MenuItem a]

data MenuOptions a = MenuOptions
    { mTitle :: String
    , mItems :: Menu a
    , mSize :: Int
    }

runMenu :: MenuOptions a -> IO a
runMenu options =
    getWidth options >>=
    printMenuOf (mTitle options)
                (mItems options)

printBar :: Int -> Int -> IO ()
printBar curv maxv =
    putStrLn (
        "[" ++
        (replicate curv '|') ++
        (replicate (maxv - curv) '.') ++
        "]"
    )

printDivider :: MenuOptions a -> IO ()
printDivider options =
    getWidth options >>=
    printDividerOf

printDividerOf :: Int -> IO ()
printDividerOf x =
    putStr $ fold (replicate x "-")

-- private

getWidth :: MenuOptions a -> IO Int
getWidth options = do
    winM <- size
    return $ case winM of
        Just win -> width win
        Nothing -> mSize options

printMenuOf :: String -> Menu a -> Int -> IO a
printMenuOf title items x =
    printDividerOf x >>
    putStrLn ("| " ++ title ++ ":") >>
    printDividerOf x >>
    printOptions items >>
    printDividerOf x >>
    requestItem items

requestItem :: Menu a -> IO a
requestItem items =
    prompt ":> " >>= processInput where
    processInput str =
        case U.tryRead str of
            Just idx -> tryGetItem str idx items
            Nothing ->
                putStrLn ("Input is not an integer: " ++ str) >>
                requestItem items

tryGetItem :: String -> Int -> Menu a -> IO a
tryGetItem str idx items =
    case U.tryIdx idx items of
        Just (_, _, isAvailable, item) ->
            if isAvailable
            then return item
            else putStrLn ("Option " ++ str ++ " is not available") >>
                 requestItem items
        Nothing ->
            putStrLn ("Index should be between 0 and " ++ show (length items - 1)) >>
            requestItem items

printOptions :: Menu a -> IO ()
printOptions xs =
    foldl (>>) (return ()) (U.mapi showItem xs) >>
    setSGR [Reset]

showItem :: MenuItem a -> Int -> IO ()
showItem (name, description, isAvailable, _) idx =
    setSGR [SetColor Foreground Vivid color] >>
    putStrLn (show idx ++ ") " ++ name ++ descr)
    where descr = if description == "" then "" else " (" ++ description ++ ")"
          color = if isAvailable then White else Black
