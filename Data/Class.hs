module Ren.Data.Class where

class Monoid m => Group m where
    invert :: m -> m
    -- mappend x (invert x) == mempty

class Bifunctor f where
    bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
    -- bimap id id == id

class Comonad w where
    extract :: w a -> a
    extend :: (w a -> b) -> w a -> w b
    -- extend . extract == id
