module Logic where

import World
import Utils
import Game
import Graphics
import Types

import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Extra.Keys

gameLoop :: Surface -> Font -> World -> IO World
gameLoop surface font world = do
    event  <- pollEvent
    setCaption ("Level 0 (" ++ show (score world) ++ ")") ""
    
    case event of
        KeyDown (Keysym SDLK_RETURN _ _) -> getStringAndDo print >>= \l -> putStr (l ++ " returned")
        _ -> return ()

    case eventHandler event world of
        -- the game died; return it
        Left (End deadWorld) -> return deadWorld

        -- need to get more random numbers dude!
        Left newWorld -> do
            start <- fmap (\(P x y) -> (P x y)) (randomXY newWorld)
            SDL.flip surface
            delay (speed world)
            drawWorld surface font (newWorld { item = Bonus [start] })
            gameLoop  surface font (newWorld { item = Bonus [start] })
            

        Right newWorld ->
            if dead newWorld -- if the snake died...
                -- display scores, wait until space bar, then start a new game
                then do
                    let scoredWorld = newWorld { scores = score newWorld : scores newWorld }
                    
                    -- starting item X and Y
                    start <- randomXY newWorld

                    drawScores surface font scoredWorld

                    SDL.flip surface

                    -- wait until space is pressed
                    waitForPress <- while3 waitEventBlocking $ \ev -> case ev of
                        KeyDown (Keysym SDLK_SPACE _ _) -> A -- case A -> stop waiting; start game
                        Quit                            -> B -- case B -> stop waiting; quit game
                        _                               -> C -- else loop again

                    -- if the user wants to quit ...
                    if waitForPress
                        then return scoredWorld
                        else do                   
                            drawWorld surface font scoredWorld
                            gameLoop  surface font (startWorld (P 16 16) start (scores scoredWorld) (fscores scoredWorld) (stage world) (speed world))

                else do
                    SDL.flip surface
                    delay (speed world)
                    drawWorld surface font newWorld
                    gameLoop  surface font newWorld

    
eventHandler :: Event -> World -> Either World World
eventHandler event world = case event of

    KeyDown (Keysym key _ _) -> Right $ case key of
        SDLK_DOWN   -> world { snake  = updateSnake stg (dir d S) s 0 pause              }
        SDLK_UP     -> world { snake  = updateSnake stg (dir d N) s 0 pause              }
        SDLK_LEFT   -> world { snake  = updateSnake stg (dir d W) s 0 pause              }
        SDLK_RIGHT  -> world { snake  = updateSnake stg (dir d E) s 0 pause              }
        SDLK_p      -> world { paused = not pause                                        }
        SDLK_k      -> world { speed  = speed world + 1                                  }
        SDLK_j      -> world { speed  = if speed world == 0 then 0 else speed world - 1  }
        _           -> world { snake  = updateSnake stg  d        s 0 pause              }

    -- if they quit, return the world in order to clean up and write the scores to a file

    Quit -> Left (End world)
    _    -> if pause
        then Right world
        else (if gotItem then Left else Right) world
            { snake = updateSnake (stage world) (direction s) s (fromEnum gotItem) False
            , item  = if gotItem then Bonus [] else item world -- so we don't impurify our precious event handler!
            , score = fromEnum gotItem + score world
            }
  where
    -- handy abbreviations
    d     = direction (snake world)
    s     = snake world
    stg   = stage world
    pause = paused world

    gotItem = updateItem (item world) s

