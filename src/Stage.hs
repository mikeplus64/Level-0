module Stage where
-- this module provides functions to create level_0 stages
import Data.Maybe (catMaybes)

import Types
import Utils
import Game

fullBlankStage :: [String]
fullBlankStage = replicate blocksWH $ replicate blocksWH ' '

fileToStage :: FilePath -> IO Stage
fileToStage path = do
    -- read file, convert to list of lines
    file <- fmap lines (readFile path)

    -- convert to stage
    return $ concatMap catMaybes $ forXY file $ \d (x, y) -> case d of
        'x' -> Just (P x y)
        _   -> Nothing

stageToString :: Stage -> [String]
stageToString stage' = forXY fullBlankStage (\_ (x, y) -> if P x y `elem` stage' then 'x' else ' ')

stageToFile :: Stage -> FilePath -> IO ()
stageToFile stage' path = 
    let stageString = unlines $ stageToString stage'
    in writeFile path stageString
