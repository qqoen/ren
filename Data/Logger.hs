module Ren.Data.Logger
    ( Logger(..)
    , logger
    ) where

data Logger a = Logger a [String]

instance Monad Logger where
    return = pure
    (>>=) (Logger val1 logs1) fn =
        let (Logger val2 logs2) = fn val1
        in Logger val2 (logs1 ++ logs2)

instance Applicative Logger where
    pure a = Logger a []
    (<*>) (Logger fn logs1) (Logger val logs2) =
        Logger (fn val) (logs1 ++ logs2)

instance Functor Logger where
    fmap fn (Logger val logs) = Logger (fn val) logs

logger :: a -> [String] -> Logger a
logger x logs = Logger x logs
