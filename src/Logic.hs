module Logic where

import World
import Utils
import Game
import Graphics
import Types
import Stage

import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Extra.Keys

import Data.List (nub, delete)
import Control.Monad (unless, void)

gameLoop :: Surface -> Font -> World -> IO World
gameLoop surface font world = do
    event <- pollEvent
    case eventHandler event world of
        -- if we want to save the map ...
        (World _ _ _ _ _ _ _ _ _ _ [SaveMap] _) -> do
            maybePath <- getStringAndDo $ \s -> do 
                drawWorld surface font world
                drawText  surface font "Filename: " (-h2 + 8) (-60)
                unless (null s) $ void $ drawText surface font s (-h2 + 8) 30
                SDL.flip surface

            case maybePath of
                Just path -> stageToFile (stage world) path
                Nothing   -> putStrLn "error: no filename entered"

            gameLoop  surface font (world { pending = [] })

        -- if the help menu is open ...
        (World _ _ _ _ _ _ _ _ _ _ _ True) -> do
            fillRect  surface (Just (Rect 0 0 windowWidth yoffset)) background
            drawText  surface font "m: editor; w: save map; j/k: +/- speed" (-h2 + 6) (-w2 + 34)
            SDL.flip  surface
            
            quitOrContinue <- while3 (delay 16 >> waitEventBlocking) $ \event0 -> case event0 of
                KeyDown (Keysym SDLK_h _ _) -> A -- continue
                Quit                        -> B -- quit
                _                           -> C -- keep waiting
            
            if quitOrContinue
                then return world
                else do
                    SDL.flip  surface
                    gameLoop  surface font world

        -- if we quit...
        w@(World False _ _ _ _ _ _ _ _ _ _ _) -> return w

        -- need to get more random numbers dude!
        newWorld@(World _ _ _ _ _ _ _ _ Nothing _ _ _) -> do
            start <- fmap (\(P x y) -> (P x y)) (randomXY newWorld)
            SDL.flip surface
            unless (editmode world) $ delay (speed world)
            drawWorld surface font (newWorld { item = Just start })
            gameLoop  surface font (newWorld { item = Just start })

        newWorld ->
            if alive (snake newWorld) -- if the snake died...
                then do
                    SDL.flip surface
                    unless (editmode world) $ delay (speed world)
                    drawWorld surface font newWorld
                    gameLoop  surface font newWorld
                -- display scores, wait until space bar, then start a new game
                else do
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
                            gameLoop  surface font $ startWorld (P 16 16) start (scores scoredWorld) (fscores scoredWorld) (stage world) (speed world)
 
eventHandler :: Event -> World -> World
eventHandler event world = if editmode world 
    then case event of
        -- place a wall on the stage
        MouseButtonDown x y ButtonLeft  -> world { stage = nub $ P (fromIntegral (x `div` 16)) (fromIntegral ((y - 24) `div` 16)) : stage world }

        -- remove a wall from the stage
        MouseButtonDown x y ButtonRight -> world { stage = delete (P (fromIntegral (x `div` 16)) (fromIntegral ((y - 24) `div` 16))) (stage world) }

        KeyDown (Keysym key _ _) -> case key of
            SDLK_m -> world { editmode = False                      }
            SDLK_h -> world { help     = not (help world)           }
            SDLK_w -> world { editmode = False, pending = [SaveMap] }
            _      -> world
        Quit -> world { running = False }
        _    -> world

    else case event of
        KeyDown (Keysym key _ _) -> case key of
            SDLK_h      -> world { help     = not (help world)                              }
            SDLK_m      -> world { editmode = True                                          }
            SDLK_r      -> world { item     = Nothing, score = score world - 1              }
            SDLK_DOWN   -> world { snake  = updateSnake stg (dir d S) s 0 pause             }
            SDLK_UP     -> world { snake  = updateSnake stg (dir d N) s 0 pause             }
            SDLK_LEFT   -> world { snake  = updateSnake stg (dir d W) s 0 pause             }
            SDLK_RIGHT  -> world { snake  = updateSnake stg (dir d E) s 0 pause             }
            SDLK_p      -> world { paused = not pause                                       }
            SDLK_k      -> world { speed  = speed world + 1                                 }
            SDLK_j      -> world { speed  = if speed world == 0 then 0 else speed world - 1 }
            _           -> world { snake  = updateSnake stg  d        s 0 pause             }

        -- if they quit, return the world in order to clean up and write the scores to a file
        Quit -> world { running = False }
        _    -> if pause
            then world
            else world
                { snake = updateSnake stg d s (fromEnum gotItem) False
                , item  = if gotItem then Nothing else item world -- so we don't impurify our precious event handler!
                , score = fromEnum gotItem + score world
                }
  where
    -- handy abbreviations
    d     = direction (snake world)
    s     = snake world
    stg   = stage world
    pause = paused world

    gotItem = updateItem (item world) s

