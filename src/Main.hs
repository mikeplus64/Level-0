module Main where

import Logic
import World
import Game
import Graphics
import Types
import Utils
import Stage

import Graphics.UI.SDL     as SDL
import Graphics.UI.SDL.TTF as TTF
import Control.Monad (when, forM_, unless)
import System.Directory (getAppUserDataDirectory, createDirectoryIfMissing, doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs

    -- make sure the conf directory exists...
    dataDir <- getAppUserDataDirectory "config/level_0"
    createDirectoryIfMissing True dataDir

    -- create files if missing
    mapExists <- doesFileExist $ dataDir ++ "/map"
    unless mapExists $ do
        putStrLn $ "Creating " ++ dataDir ++ "/map" 
        writeFile (dataDir ++ "/map") ""

    scoreExists <- doesFileExist $ dataDir ++ "/score"
    unless scoreExists $ do
        putStrLn $ "Creating " ++ dataDir ++ "/score"
        writeFile (dataDir ++ "/score") ""
    

    (speed', stage'') <- case args of
        ["none", speed'']   -> return (read speed'', return [])
        [path, speed'']     -> return (read speed'', fileToStage path)
        [path]              -> return (16, fileToStage path)
        []                  -> return (16, return [])
        _                   -> error "usage: level_0 [stage file [speed]|stage file]. If you want to set a custom speed, you must also set the map file. For no map use 'none'"

    stage' <- stage''
    
    -- start your engines
    SDL.init [InitEverything]
    TTF.init
    
    let defaultFont = (dataDir ++ "/font.ttf", "/usr/share/fonts/TTF/TerminusBold.ttf")

    defaultFontExist <- doesFileExist $ fst defaultFont
    unless defaultFontExist $ putStrLn "No default font found. Trying /usr/share/fonts/TTF/TerminusBold.ttf."

    terminusFontExist <- doesFileExist $ snd defaultFont
    unless (defaultFontExist || terminusFontExist) $ putStrLn "/usr/share/fonts/TTF/TerminusBold.ttf doesn't exist. Please enter the path of a font: "
    
    fontPath <- case (defaultFontExist, terminusFontExist) of
                (True, _) -> return $ fst defaultFont
                (_, True) -> return $ snd defaultFont
                _ -> 
                    let loop = do
                        fpath <- getLine
                        exists <- doesFileExist fpath
                        if exists
                            then return fpath
                            else do
                                putStrLn "Font does not exist. Try again."
                                loop
                    in loop

    font <- openFont fontPath 18

    setCaption "Level 0" ""
    setVideoMode windowWidth windowHeight 24 [HWSurface, DoubleBuf]
    surface <- getVideoSurface

    start <- randomXY (startWorld (P 16 16) (P 0 0) [] [] stage' 0)

    -- display intro
    drawWorld surface font (startWorld (P 16 16) start [] [] stage' 0)
    drawText  surface font "Press space to begin." 0 (-60)
    SDL.flip surface

    -- catch whether or not the user wants to quit at the start menu
    playGame <- while3 waitEventBlocking $ \event -> case event of
        KeyDown (Keysym SDLK_SPACE _ _) -> B
        Quit                            -> A
        _                               -> C

    when playGame $ do
        oldScores' <- fmap lines $ readFile (dataDir ++ "/score")

        -- force the buffer to close
        length oldScores' `seq` return ()

        let oldScores = map (\x -> read x :: Int) oldScores'

        game <- gameLoop surface font (startWorld (P 16 16) start [] oldScores stage' speed')

        SDL.quit

        -- write score to a file
        forM_ (map ((++ "\n") . show) (scores game)) $ \score' ->
            appendFile (dataDir ++ "/score") score'

    SDL.quit
