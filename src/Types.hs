module Types where
{-# LANGUAGE BangPatterns #-}

data Point = P !Int !Int deriving Eq

type Stage = [Point]

data Item = Bonus [Point]

-- North, South, East, West
data Direction = N | S | E | W

{-# INLINE direction #-}
direction :: Snake -> Direction
direction (Snake d _) = d

data Snake = Snake Direction [Point] | Dead Direction [Point]

data World = World {
          snake     :: Snake
        , stage     :: Stage
        , paused    :: Bool
        , score     :: Int
        , scores    :: [Int]
        , fscores   :: [Int]
        , item      :: Item
    } | End World


class Points a where
    points :: a -> [Point]

instance Points Snake where
    points (Dead  _ ps) = ps
    points (Snake _ ps) = ps

instance Points Item where
    points (Bonus xs) = xs
