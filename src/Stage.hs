module Stage where
-- this module provides functions to create level_0 stages
import Data.Maybe (catMaybes)

import Types
import Utils
import Game

data FSBlock = Blank (Int, Int) | Wall (Int, Int)
    deriving Show

type FullStage = [[FSBlock]]

fullBlankStage :: FullStage
fullBlankStage = forXY uncoord (\v (x, y) -> v (x, y))
    where
        uncoord = replicate blocksWH $ replicate blocksWH $ Blank

fileToStage :: FilePath -> IO Stage
fileToStage path = do
    -- read file, convert to list of lines
    file <- fmap lines (readFile path)

    -- convert to stage
    return $ concatMap catMaybes $ forXY file $ \d (x, y) -> case d of
        'x' -> Just (P x y)
        _   -> Nothing
