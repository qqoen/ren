module Ren.Data.Class where

class Monoid m => Group m where
    invert :: m -> m

class Bifunctor f where
    bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
