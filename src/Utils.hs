module Utils where

data Ternary = A | B | C

forXY :: [[a]] -> (a -> (Int, Int) -> b) -> [[b]]
forXY list f = map (\(y, line') -> map (\(x, element) -> f element (x, y)) line') (zip [0..] (map (zip [0..]) list))

while3 :: IO a -> (a -> Ternary) -> IO Bool
while3 action check = do
    result <- action
    case check result of
        A -> return False
        B -> return True
        C -> while3 action check
