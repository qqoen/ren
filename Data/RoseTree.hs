module Ren.Data.RoseTree where

data RoseTree a = Node a [RoseTree a]
