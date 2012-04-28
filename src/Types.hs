module Types where
import Data.Word (Word32)
{-# LANGUAGE BangPatterns #-}

data Point = P !Int !Int deriving Eq

type Stage = [Point]

type Item = Maybe Point

-- North, South, East, West
data Direction = N | S | E | W

data GameMode = Game | End | Save GameMode | Load GameMode | Editor | Scoreboard
  deriving Eq

data Snake = Snake
    { alive     :: Bool
    , direction :: Direction 
    , points    :: [Point] 
    }

data World = World 
    { snake     :: Snake
    , stage     :: Stage
    , speed     :: Word32
    , score     :: Int
    , scores    :: [Int]
    , fscores   :: [Int]
    , item      :: Item
    , mode      :: (Bool, GameMode) -- whether the help menu is open and the current game mode
    }
