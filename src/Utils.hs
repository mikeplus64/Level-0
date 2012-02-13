module Utils where

data Ternary = A | B | C

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

doUntil :: IO a -> (a -> Ternary) -> IO Bool
doUntil action check = do
    result <- action
    case check result of
        A -> return False
        B -> return True
        C -> doUntil action check
