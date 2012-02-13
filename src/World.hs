module World where

import Graphics.UI.SDL.Color
import Control.Arrow ((***))
import Utils
import Game
import Types
import Data.List (nub)
import System.Random (randomRIO)

import Debug.Trace

-- get random (Int, Int)

randomXY :: World -> IO (Int, Int)
randomXY world = do
    r0 <- randomRIO (1, 31)
    r1 <- randomRIO (1, 31)
    
    if any (== (r0, r1)) (stage world)
        then trace "randomXY: collision:" (randomXY world)
        else return (r0, r1)

-- if the item is eaten, return (True, the item's coordinates)
-- else, return (False, the item's coordinates)

updateItem :: Item -> Snake -> (Bool, Item)
updateItem item' snake = case safeHead (points item') of
    Just i  -> if any (== i) (points snake)
        then (True,  item')
        else (False, item')
    Nothing -> (False, item')

-- if the old and new direction is D, then don't do anything
-- if the direction is anything else, and we aren't adding to the snake, then move the snake and check for collisions
-- otherwise try and move it and insert x new blocks in the snake.

updateSnake :: Stage -> Direction -> Snake -> Int -> Bool -> Snake
updateSnake _     _ d@(Dead  _ _ ) _ _ = d
updateSnake stage D s@(Snake _ ps) _ b = if b then s else Snake D ps
updateSnake stage d s@(Snake _ ps) 0 b = if b then s else collision stage $ Snake d $ moveD d (head ps) : init ps
updateSnake stage d s@(Snake _ ps) x b = if b then s else collision stage $ Snake d $ moveD d (head ps) : replicate x (head ps) ++ init ps

-- check for collisions, if there is one, empty the snake
collision :: Stage -> Snake -> Snake
collision stage s@(Snake d ps) = case d of
    D -> s
    _ -> if any (== head ps) (tail ps ++ stage)
            then Dead d ps
            else s

moveD :: Direction -> Point -> Point
moveD direction (x, y) = wrap rawCoordinate
  where
    -- raw coordinate without trying to loop around the edge of the screen
    wrap :: Point -> Point
    wrap (x, y) = (mod x windowWidth, mod y windowHeight)

    rawCoordinate :: Point
    rawCoordinate = case direction of
        N -> moveP (0, (-bs))
        S -> moveP (0,    bs)
        E -> moveP (bs,    0)
        W -> moveP ((-bs), 0)
        D -> (x, y)
    
    moveP :: Point -> Point
    moveP (x0, y0) = (x0 + x, y0 + y)

-- make sure the snake isn't trying to turn into itself
dir :: Direction -> Direction -> Direction
dir oldDir newDir = case oldDir of
    D -> newDir
    N -> case newDir of
        S -> N
        _ -> newDir
    S -> case newDir of
        N -> S
        _ -> newDir
    E -> case newDir of
        W -> E
        _ -> newDir
    W -> case newDir of
        E -> W
        _ -> newDir

dead :: World -> Bool
dead world = case snake world of
    Dead  _ _  -> True
    _          -> False
