module Utils where

data Ternary = A | B | C

mapXY :: (a -> (Int, Int) -> b) -> [[a]] -> [[b]]
mapXY f list = map (\(y, line') -> map (\(x, element) -> f element (x, y)) line') (zip [0..] (map (zip [0..]) list))

doUntil :: IO a -> (a -> Ternary) -> IO Bool
doUntil action check = do
    result <- action
    case check result of
        A -> return False
        B -> return True
        C -> doUntil action check
