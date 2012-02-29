module Game where
import Graphics.UI.SDL.Color
import Data.Word (Word32)
import Types

windowWidth, windowHeight, w2, h2, halfHeight, halfWidth :: Int
windowWidth  = 512
windowHeight = 512 + yoffset
w2 = windowWidth  `div` 2
h2 = windowHeight `div` 2

yoffset :: Int
yoffset = 24

halfWidth  = windowWidth  `div` 2
halfHeight = windowHeight `div` 2

background, lightColour, shadowColour, darkColour :: Pixel
background   = Pixel 0xC5DBC1
lightColour  = Pixel 0xA8B6A4
shadowColour = Pixel 0x697D62
darkColour   = Pixel 0x222422

blockSize :: Int
blockSize = 16

blocksWH :: Int
blocksWH = windowWidth `div` blockSize

bs, bs2, bs4 :: Int
bs  = blockSize
bs2 = blockSize `div` 2
bs4 = blockSize `div` 4

startWorld :: Point -> Point -> [Int] -> [Int] -> Stage -> Word32 -> World
startWorld (P sX sY) (P iX iY) scores' fscores' stage' speed' = 
    World 
    { snake    = Snake True E [P sX sY]
    , stage    = stage'
    , paused   = False
    , score    = 0
    , speed    = speed'
    , scores   = scores'
    , fscores  = fscores'
    , item     = Just (P iX iY)
    , running  = True
    , editmode = False
    , pending  = []
    , help     = False
    }
