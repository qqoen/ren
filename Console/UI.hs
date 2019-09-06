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
import Control.Monad.Fix (fix)

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
    printMenuOf (mTitle options)
                (mItems options)
    =<< getWidth options

printBar :: Int -> Int -> IO ()
printBar curv maxv =
    putStrLn (
        "[" ++
        (replicate curv '|') ++
        (replicate (maxv - curv) '.') ++
        "]"
    )

printDivider :: MenuOptions a -> IO ()
printDivider =
    (printDividerOf =<<) . getWidth

printDividerOf :: Int -> IO ()
printDividerOf =
    putStr . fold . (flip replicate "-")

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
requestItem = fix $ \recfn items -> do
    input <- prompt ":> "

    case pickItem items input of
        Right a -> return a
        Left err ->
            putStrLn err >>
            recfn items

pickItem :: Menu a -> String -> Either String a
pickItem items str =
    case U.tryRead str of
        Just idx ->
            case U.tryIdx idx items of
                Just (_, _, isAvailable, item) ->
                    if isAvailable
                    then Right item
                    else Left ("Option " ++ str ++ " is not available")

                Nothing ->
                    Left ("Index should be between 0 and " ++ show (length items - 1))
        Nothing ->
            Left ("Input is not an integer: " ++ str)

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
