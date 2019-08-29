{-# LANGUAGE RankNTypes #-}

module Ren.Lens where

import Control.Applicative
import Data.Functor.Identity

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

type Traversal s a = forall f. Applicative f => (a -> f a) -> s -> f s

get :: Lens s a -> (s -> a)
get ln = getConst . ln Const

set :: Lens s a -> (a -> s -> s)
set ln x = runIdentity . ln (Identity . const x)


