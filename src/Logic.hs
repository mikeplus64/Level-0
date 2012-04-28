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
import Data.Maybe (fromMaybe)
import Control.Monad (unless, void)
import Control.Applicative ( (<$>) )
import Data.Foldable (for_)
import Data.Traversable (forM)

paused :: World -> Bool
paused w = snd (mode w) == Scoreboard

editmode :: World -> Bool
editmode w = snd (mode w) == Editor

getFileName :: Surface -> Font -> World -> IO (Maybe String)
getFileName surface font world = getStringAndDo $ \s -> do 
    drawWorld surface font world
    drawText  surface font "Filename: " (-h2 + 8) (-110)
    unless (null s) $ void $ drawText surface font s (-h2 + 8) 30
    SDL.flip surface

gameLoop :: Surface -> Font -> World -> IO World
gameLoop surface font world = do
    event <- pollEvent
    case eventHandler event world of
        -- if we quit...
        w@(World { mode = (_, End) }) -> return w
        
        -- if we want to save the map ...
        World { mode = (_, Save previous) } -> do
            maybePath <- getFileName surface font world
            for_ maybePath $ stageToFile (stage world)
            gameLoop  surface font (world { mode = (False, previous) })

        -- if we want to load a map ...
        World { mode = (_, Load previous) } -> do
            maybePath <- getFileName surface font world
            newStage  <- fromMaybe (stage world) <$> forM maybePath fileToStage
            gameLoop  surface font (world { mode = (False, previous), stage = newStage } )

        -- if the help menu is open ...
        World { mode = (True, previous) } -> do
            fillRect  surface (Just (Rect 0 0 windowWidth (yoffset + 15))) shadowColour
            fillRect  surface (Just (Rect 0 0 windowWidth (yoffset + 13))) darkColour
            fillRect  surface (Just (Rect 0 0 windowWidth (yoffset + 12))) background
            drawText  surface font "m: editor; w: save map; c: clear; j/k: +/- speed" (-h2 + 3) (-w2 + 34)
            drawText  surface font "l: load map; h: toggle this help" (-h2 + 19) (-w2 + 34)
            SDL.flip  surface
            
            quitOrContinue <- while3 (delay 16 >> waitEventBlocking) $ \event0 -> case event0 of
                KeyDown (Keysym SDLK_h _ _) -> A -- continue
                Quit                        -> B -- quit
                _                           -> C -- keep waiting
            
            if quitOrContinue
                then return world
                else do
                    SDL.flip  surface
                    gameLoop  surface font (world { mode = (False, previous) })

        -- need to get more random numbers dude!
        newWorld@(World { item = Nothing }) -> do
            start <- randomXY newWorld
            SDL.flip surface
            unless (editmode world) $ delay (speed world)
            drawWorld surface font $ newWorld { item = Just start }
            gameLoop  surface font $ newWorld { item = Just start }

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
                    quitWhileWaitingForPress <- while3 waitEventBlocking $ \ev -> case ev of
                        KeyDown (Keysym SDLK_SPACE _ _) -> A -- case A -> stop waiting; start game
                        Quit                            -> B -- case B -> stop waiting; quit game
                        _                               -> C -- else loop again

                    -- if the user wants to quit ...
                    if quitWhileWaitingForPress
                        then return scoredWorld
                        else do                   
                            drawWorld surface font scoredWorld
                            gameLoop  surface font $ startWorld (P 16 16) start (scores scoredWorld) (fscores scoredWorld) (stage world) (speed world)
 
eventHandler :: Event -> World -> World
eventHandler event world = case mode world of
    (_, Editor) -> case event of
        -- place a wall on the stage
        MouseButtonDown x y ButtonLeft  -> 
            world { stage = nub $ P (fromIntegral (x `div` 16)) (fromIntegral ((y - 24) `div` 16)) : stage world }

        -- remove a wall from the stage
        MouseButtonDown x y ButtonRight -> 
            world { stage = delete (P (fromIntegral (x `div` 16)) (fromIntegral ((y - 24) `div` 16))) (stage world) }

        KeyDown (Keysym key _ _) -> case key of
            SDLK_c -> world { stage = []                                    }
            SDLK_m -> world { mode  = (False, Game)                         }
            SDLK_h -> world { mode  = (not (help world), snd $ mode world)  }
            SDLK_l -> world { mode  = (False, Load Editor)                  }
            SDLK_w -> world { mode  = (False, Save Editor)                  }
            _      -> world

        Quit -> world { mode = (False, End) }
        _    -> world

    (_, Game) -> case event of
        KeyDown (Keysym key _ _) -> case key of
            SDLK_h      -> world { mode   = (not (help world), snd $ mode world)            }
            SDLK_m      -> world { mode   = (False, Editor)                                 }
            SDLK_r      -> world { item   = Nothing, score = score world - 1                }
            SDLK_DOWN   -> world { snake  = updateSnake stg (dir d S) s 0 pause             }
            SDLK_UP     -> world { snake  = updateSnake stg (dir d N) s 0 pause             }
            SDLK_LEFT   -> world { snake  = updateSnake stg (dir d W) s 0 pause             }
            SDLK_RIGHT  -> world { snake  = updateSnake stg (dir d E) s 0 pause             }
            SDLK_p      -> world { mode   = (help world, Scoreboard)                        }
            SDLK_k      -> world { speed  = speed world + 1                                 }
            SDLK_j      -> world { speed  = if speed world == 0 then 0 else speed world - 1 }
            SDLK_l      -> world { mode   = (False, Load Game)                              }
            SDLK_w      -> world { mode   = (False, Save Game)                              }
            _           -> world { snake  = updateSnake stg  d        s 0 pause             }

        -- if they quit, return the world in order to clean up and write the scores to a file
        Quit -> world { mode = (False, End) }
        _    -> if paused world
            then world
            else world
                { snake = updateSnake stg d s (fromEnum gotItem) False
                , item  = if gotItem then Nothing else item world -- so we don't impurify our precious event handler!
                , score = fromEnum gotItem + score world
                }
    _    -> world
  where
    -- handy abbreviations
    help  = fst . mode
    d     = direction (snake world)
    s     = snake world
    stg   = stage world
    pause = paused world

    gotItem = updateItem (item world) s

