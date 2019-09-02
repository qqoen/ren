module Ren.Console.UI
    ( runMenu
    , printBar
    , printDivider
    , printDividerOf
    , Menu
    , MenuItem
    ) where

-- Console UI utilities

import System.IO

import System.Console.ANSI
import System.Console.Terminal.Size

import Ren.Utils as U
import Ren.Console.Base

-- name, description, is available, return value
type MenuItem a = (String, String, Bool, a)
type Menu a = [MenuItem a]

runMenu :: String -> Menu a -> IO a
runMenu title items =
    size >>= \sizeM ->
        case sizeM of
            Just win -> printMenuOf ((width win) - 1) title items
            Nothing -> printMenuOf defWinSize title items

printBar :: Int -> Int -> Int -> Int -> IO ()
printBar size minv maxv curv =
    let ratio = (fromIntegral curv) / (fromIntegral $ maxv - minv)
        bars = round (ratio * fromIntegral size)
        left = size - bars
    in
        putStrLn ("[" ++ (replicate bars '|') ++ (replicate left '.') ++ "]")

printDivider :: IO ()
printDivider =
    size >>= \sizeM ->
        case sizeM of
            Just win -> printDividerOf (width win)
            Nothing -> printDividerOf defWinSize

printDividerOf :: Int -> IO ()
printDividerOf x =
    putStr $ foldl (++) "" (replicate x "-")

-- Helpers

defWinSize :: Int
defWinSize = 30

printMenuOf :: Int -> String -> Menu a -> IO a
printMenuOf x title items =
    printDividerOf x >>
    putStrLn ("| " ++ title ++ ":") >>
    printDividerOf x >>
    printOptions items >>
    printDividerOf x >>
    requestItem items

requestItem :: Menu a -> IO a
requestItem items =
    let processInput str =
            case U.tryRead str of
                Just idx ->
                    case U.tryIdx idx items of
                        Just (_, _, isAvailable, item) ->
                            case isAvailable of
                                True -> return item
                                False ->
                                    putStrLn ("Option " ++ str ++ " is not available") >>
                                    requestItem items
                        Nothing ->
                            putStrLn ("Index should be between 0 and " ++ show (length items - 1)) >>
                            requestItem items
                Nothing ->
                    putStrLn ("Input is not an integer: " ++ str) >>
                    requestItem items
    in
        prompt ":> " >>= processInput

printOptions :: Menu a -> IO ()
printOptions xs =
    let showItem (name, description, isAvailable, _) idx =
            (if isAvailable
                then setSGR [SetColor Foreground Vivid White]
                else setSGR [SetColor Foreground Vivid Black]) >>
            putStrLn (show idx ++ ") " ++ name ++ if description == "" then "" else " (" ++ description ++ ")")
    in
        foldl (>>) (return ()) (U.mapi showItem xs) >>
        setSGR [Reset]


