{-# LANGUAGE ForeignFunctionInterface #-}

module Ren.Console.Base
    ( getHiddenChar
    , prompt
    ) where

-- Basic console utilities

import Data.Char
import Control.Monad (liftM)
import Foreign.C.Types
import System.IO

-- Workaround for windows console not working properly with "hSetBuffering stdin NoBuffering"
getHiddenChar :: IO Char
getHiddenChar = liftM (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
    c_getch :: IO CInt

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
