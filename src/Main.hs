module Main where

import Logic
import World
import Game
import Graphics
import Types
import Utils

import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF
import Control.Monad (forM_)
import System.Random (randomRIO)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing)
import System.Exit

main = do
    -- make sure the conf directory exists...
    dataDir <- getAppUserDataDirectory "config/level_0"
    createDirectoryIfMissing True dataDir
    
    -- start your engines
    SDL.init [InitEverything]
    TTF.init

    font <- openFont (dataDir ++ "/font.ttf") 18

    setCaption "Level 0" ""
    setVideoMode windowWidth windowWidth 24 [HWSurface, DoubleBuf]
    surface <- getVideoSurface

    -- starting snake X and Y
    sX <- randomRIO (0, 31)
    sY <- randomRIO (0, 31)
    
    -- starting item X and Y
    iX <- randomRIO (0, 31)
    iY <- randomRIO (0, 31)

    -- display intro
    drawWorld surface font (startWorld sX sY iX iY [] [])
    drawText  surface font "Press space to begin." 0 (-60)
    SDL.flip surface

    playGame <- (doUntil waitEventBlocking $ \event -> case event of
        KeyDown (Keysym SDLK_SPACE _ _) -> A
        Quit                            -> B
        _                               -> C)
    
    if not playGame
        then do
            oldScores' <- fmap lines $ readFile (dataDir ++ "/score")

            -- force the buffer to close
            length oldScores' `seq` return ()

            let oldScores = map (\x -> read x :: Int) oldScores'

            game <- gameLoop surface font (startWorld sX sY iX iY [] oldScores)
            
            SDL.quit
            
            -- write score to a file
            forM_ (map ((++ "\n") . show) (scores game)) $ \score' ->
                appendFile (dataDir ++ "/score") score'

            -- finish up
            exitWith ExitSuccess
        else do
            -- this wasn't a very meaningful existence at all
            SDL.quit
            exitWith ExitSuccess
