module World where
{-# LANGUAGE BangPatterns #-}

import Types
import Game

import System.Random (randomRIO)

-- get random (Int, Int), making sure it isn't a point in the map.
randomXY :: World -> IO Point
randomXY world = do
    x <- randomRIO (1, 31)
    y <- randomRIO (1, 31)
    if P x y `elem` stage world
        then randomXY world
        else return (P x y)
    
-- if the item is eaten, return (True, the item's coordinates)
-- else, return (False, the item's coordinates)

updateItem :: Item -> Snake -> Bool
updateItem item' snake' = case (points item') of
    [i] -> any (== i) (points snake')
    _   -> False

-- if the old and new direction is D, then don't do anything
-- if the direction is anything else, and we aren't adding to the snake, then move the snake and check for collisions
-- otherwise try and move it and insert x new blocks in the snake.

updateSnake :: Stage -> Direction -> Snake -> Int -> Bool -> Snake
-- if the snake is paused, don't update it
updateSnake _     _ s           _ True = s

-- if the snake is dead, don't update it
updateSnake _     _ d@(Dead  _ _ ) _ _ = d

-- check for collisions in the new snake
updateSnake stage' d (Snake _ ps) 0 _ = collision stage' $ Snake d $ moveD d (head ps) : init ps
updateSnake stage' d (Snake _ ps) x _ = collision stage' $ Snake d $ moveD d (head ps) : replicate x (head ps) ++ init ps

-- check for collisions, if there is one, empty the snake
collision :: Stage -> Snake -> Snake
collision stage' s@(Snake d ps) = 
    if any (== head ps) (tail ps ++ stage')
        then Dead d ps
        else s

collision _ d@(Dead _ _) = d

moveD :: Direction -> Point -> Point
moveD dir' (P x y) = wrap $ case dir' of
        N -> moveP (P 0 (-1))
        S -> moveP (P 0    1)
        E -> moveP (P 1    0)
        W -> moveP (P (-1) 0)
  where
    -- raw coordinate without trying to loop around the edge of the screen
    wrap (P x0 y0) = P (mod x0 blocksWH) (mod y0 blocksWH)
    
    moveP (P x0 y0) = P (x0 + x) (y0 + y)

-- make sure the snake isn't trying to turn into itself
dir :: Direction -> Direction -> Direction
dir oldDir newDir = case oldDir of
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
