module Stage where
-- this module provides functions to create level_0 stages
import Data.Maybe (catMaybes)

import Types
import Utils
import Game

fileToStage :: FilePath -> IO Stage
fileToStage path = do
    -- read file, convert to list of lines
    file <- fmap lines (readFile path)

    -- convert to stage
    return $ concat $ map catMaybes $ mapXY (\d (x, y) -> case d of
        'x' -> Just (P x y)
        _   -> Nothing) file
