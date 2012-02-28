module Graphics where

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.TTF as TTF
import Control.Monad (when, forM_)
import Data.List (sort)
import Types
import Game

drawScores :: Surface -> Font -> World -> IO ()
drawScores surface font world = do
    -- fill the bg shadow, border and bg
    fillRect surface (Just (Rect (w2 - 38) (h2 - 78 + yoffset) 81 161)) shadowColour
    fillRect surface (Just (Rect (w2 - 41) (h2 - 81 + yoffset) 82 162)) darkColour
    fillRect surface (Just (Rect (w2 - 40) (h2 - 80 + yoffset) 80 160)) background
    
    -- draw a pretty grid
    forM_ [0, 8 .. 72] $ \x ->
        fillRect surface (Just (Rect (x + w2 - 40) (h2 - 80 + yoffset) 1 159)) lightColour
    
    forM_ [0, 8 .. 152] $ \y ->
        fillRect surface (Just (Rect (w2 - 40) (y + h2 - 80 + yoffset) 79 1)) lightColour

    -- format the scores from the world
    -- ie make sure there aren't more than the menu can display
    -- and sort them from first to last

    let scores' = zip [1..] $ take 8 $ map show $ reverse $ sort $ scores world ++ fscores world
    
    drawText surface font "Scores" (-74 + yoffset) 0

    forM_ scores' $ \(n, score') -> 
        drawText surface font (show n ++ ". " ++ score') (n * 16 - 68 + yoffset) 0

drawText :: Surface -> Font -> String -> Int -> Int -> IO Bool
drawText surface font string n x = do
    textShadow <- renderTextSolid font string (Color 105 125 98)
    textDark   <- renderTextSolid font string (Color 34  36  34)
    blitSurface textShadow Nothing surface (Just (Rect (w2 - 28 + x) (n + h2 - 2) 82 16))
    blitSurface textDark   Nothing surface (Just (Rect (w2 - 30 + x) (n + h2 - 4) 80 14))

drawWorld :: Surface -> Font -> World -> IO ()
drawWorld surface font world = do 
    -- fill background
    fillRect surface (Just (Rect 0 0 windowWidth windowHeight)) background

    -- draw grid
    forM_ [0, bs .. windowWidth] $ \x ->
        fillRect surface (Just (Rect x yoffset 1 windowHeight)) lightColour

    forM_ [0, bs .. windowWidth] $ \y ->
        fillRect surface (Just (Rect 0 (y + yoffset) windowWidth 1)) lightColour

    -- draw snake shadows
    forM_ (points (snake world)) $ \(P x y) -> 
        fillRect surface (Just (Rect (x * bs + 2) (y * bs + 2 + yoffset) bs bs)) shadowColour
    
    -- draw stage shadow
    forM_ (stage world) $ \(P x y) ->
        fillRect surface (Just (Rect (x * bs + 2) (y * bs + 2 + yoffset) bs bs)) shadowColour
    
    -- draw stage
    forM_ (stage world) $ \(P x y) -> 
        fillRect surface (Just (Rect (x * bs ) (y * bs + yoffset) bs bs)) darkColour
    
    -- draw items
    forM_ (points (item world)) $ \(P x y) -> do
        fillRect surface (Just (Rect (x * bs + bs4 + 2) (y * bs + bs4 + 2 + yoffset) bs2 bs2)) shadowColour
        fillRect surface (Just (Rect (x * bs + bs4)     (y * bs + bs4 + yoffset)     bs2 bs2)) darkColour
    
    -- draw snake
    forM_ (points (snake world)) $ \(P x y) -> 
        fillRect surface (Just (Rect (x * bs) (y * bs + yoffset) bs bs)) darkColour
    
    -- draw the score
    drawText surface font (show (score world)) (8 - h2) (32 - w2)
    
    -- draw speed setting
    drawText surface font (show (speed world) ++ "ms") (8 - h2) (80 - w2)
    
    -- draw top edge shadow
    fillRect surface (Just (Rect 0 (yoffset + 1) windowWidth 2)) shadowColour

    -- draw top edge
    fillRect surface (Just (Rect 0 yoffset windowWidth 1)) darkColour
    
    -- if the world is paused, draw the score board
    when (paused world) $ drawScores surface font world

