module Types where

type Point = (Int, Int)

type Stage = [Point]

data Item = Bonus [Point]

-- North, South, East, West or Dennis
data Direction = N | S | E | W | D

data Snake = Snake Direction [Point]

data World = World {
          snake     :: Snake
        , stage     :: Stage
        , paused    :: Bool
        , score     :: Int
        , scores    :: [Int]
        , fscores   :: [Int]
        , item      :: Item
    }


class Points a where
    points :: a -> [Point]

instance Points Snake where
    points (Snake _ ps) = ps

instance Points Item where
    points (Bonus xs) = xs
