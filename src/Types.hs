module Types where
import Data.Word (Word32)
{-# LANGUAGE BangPatterns #-}

data Point = P !Int !Int deriving Eq

type Stage = [Point]

type Item = Maybe Point

-- North, South, East, West
data Direction = N | S | E | W

data Snake = Snake
    { alive     :: Bool
    , direction :: Direction 
    , points    :: [Point] 
    }

data World = World 
    { running   :: Bool
    , snake     :: Snake
    , stage     :: Stage
    , paused    :: Bool
    , speed     :: Word32
    , score     :: Int
    , scores    :: [Int]
    , fscores   :: [Int]
    , item      :: Item
    }
