module World where

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
updateItem Nothing  _       = False
updateItem (Just p) snake'  = p `elem` points snake'

-- if the old and new direction is D, then don't do anything
-- if the direction is anything else, and we aren't adding to the snake, then move the snake and check for collisions
-- otherwise try and move it and insert x new blocks in the snake.

updateSnake :: Stage -> Direction -> Snake -> Int -> Bool -> Snake
-- if the snake is paused, don't update it
updateSnake _     _ s           _ True = s

-- if the snake is dead, don't update it
updateSnake _     _ d@(Snake False  _ _ ) _ _ = d

-- check for collisions in the new snake
updateSnake stage' d (Snake True _ ps) 0 _ = collision stage' $ Snake True d $ moveD d (head ps) : init ps
updateSnake stage' d (Snake True _ ps) x _ = collision stage' $ Snake True d $ moveD d (head ps) : replicate x (head ps) ++ init ps

-- check for collisions, if there is one, empty the snake
collision :: Stage -> Snake -> Snake
collision stage' s@(Snake _ _ ps) = 
    if head ps `elem` tail ps ++ stage'
        then s { alive = False }
        else s

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
