module Game where
import Graphics.UI.SDL.Color
import Types

windowWidth, windowHeight :: Int
windowWidth  = 512
windowHeight = 512
w2 = windowWidth  `div` 2
h2 = windowHeight `div` 2

halfWidth  = windowWidth  `div` 2
halfHeight = windowHeight `div` 2

background   = Pixel 0xC5DBC1
lightColour  = Pixel 0xA8B6A4
shadowColour = Pixel 0x697D62
darkColour   = Pixel 0x222422

blockSize :: Int
blockSize = 16

blocksWH  = windowWidth `div` blockSize

bs  = blockSize
bs2 = blockSize `div` 2
bs4 = blockSize `div` 4

startWorld :: Int -> Int -> Int -> Int -> [Int] -> [Int] -> Stage -> World
startWorld sX sY iX iY scores' fscores' stage' = 
    World {
          snake   = Snake E [(sX * bs, sY * bs)]
        , stage   = stage'
        , paused  = False
        , score   = 0
        , scores  = scores'
        , fscores = fscores'
        , item    = Bonus [(iX * bs, iY * bs)]
    }


