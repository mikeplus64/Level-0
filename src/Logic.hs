module Logic where

import World
import Utils
import Game
import Graphics
import Types

import Control.Monad (unless)
import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF

gameLoop :: Surface -> Font -> World -> IO World
gameLoop surface font world = do
    event    <- pollEvent
    (r0, r1) <- randomXY world
    case eventHandler (r0, r1) event world of

        -- the game died; return it
        Left deadWorld -> return deadWorld

        Right newWorld -> do
            if dead newWorld -- if the snake died...
                -- display scores, wait until space bar, then start a new game
                then do
                    let scoredWorld = newWorld { scores = (score newWorld):(scores newWorld) }
                    
                    -- starting item X and Y
                    (iX, iY) <- randomXY world

                    drawScores surface font scoredWorld

                    SDL.flip surface

                    -- wait until space is pressed
                    waitForPress <- doUntil waitEventBlocking $ \event -> case event of
                        KeyDown (Keysym SDLK_SPACE _ _) -> A
                        Quit                            -> B
                        _                               -> C

                    -- if the user wants to quit ...
                    if waitForPress
                        then return scoredWorld
                        else do                   
                            drawWorld surface font scoredWorld
                            gameLoop  surface font (startWorld 16 16 iX iY (scores scoredWorld) (fscores scoredWorld) (stage world))

                -- 
                else do 
                    SDL.flip surface
                    delay 16
                    drawWorld surface font newWorld
                    gameLoop  surface font newWorld

    
eventHandler :: (Int, Int) -> Event -> World -> Either World World
eventHandler (r0, r1) event world = case event of

    KeyDown (Keysym key _ _) -> Right $ case key of
        SDLK_DOWN  -> world { snake  = updateSnake stg (dir d S) s 0 p }
        SDLK_UP    -> world { snake  = updateSnake stg (dir d N) s 0 p }
        SDLK_LEFT  -> world { snake  = updateSnake stg (dir d W) s 0 p }
        SDLK_RIGHT -> world { snake  = updateSnake stg (dir d E) s 0 p }
        SDLK_p     -> world { paused = not p                       }
        _          -> world { snake  = updateSnake stg  d        s 0 p }

    -- if they quit, return the world in order to clean up and write the scores to a file
    Quit -> Left  world
    _    -> Right $ if p then world else world
                { snake = updateSnake stg d s (fromEnum itemGet) False
                -- if the item is eaten, make a new one.
                , item = if itemGet 
                            then Bonus [(r0 * bs, r1 * bs)] 
                            else itemGot 
                -- if an item was eaten, add 1 to the score
                , score = fromEnum itemGet + (score world)
                }
  where
    -- handy abbreviations
    s   = snake world
    d   = (\(Snake dir _) -> dir) s 
    p   = paused world
    stg = stage world

    (itemGet, itemGot) = updateItem (item world) s

