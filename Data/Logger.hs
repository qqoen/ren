module Ren.Data.Logger
    ( Logger(..)
    ) where

data Logger a = Logger a [String]

instance Monad Logger where
    return a = Logger a []
    (>>=) (Logger val1 logs1) fn =
        let (Logger val2 logs2) = fn x
        in Logger val2 (logs1 ++ logs2)
