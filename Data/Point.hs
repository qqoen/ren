module Ren.Data.Point where

import Ren.Data.Class

newtype Point = Point (Int, Int)

instance Semigroup Point where
    Point (x1, y1) <> Point (x2, y2) = Point (x1 + x2, y1 + y2)

instance Monoid Point where
    mempty = zero

instance Group Point where
    invert (Point (x, y)) = Point (-x, -y)

map2 f1 f2 (Point (x, y)) = Point (f1 x, f2 y)

-- Basic points

up    = Point (0, -1)
down  = Point (0, 1)
left  = Point (-1, 0)
right = Point (1, 0)
one   = Point (1, 1)
zero  = Point (0, 0)
