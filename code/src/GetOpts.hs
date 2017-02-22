module GetOpts
    ( getOpts
    ) where

import qualified Types as Types

options :: [Char]
options = ['h', 'd', 'x', 'u']

goodFlags :: [Char] -> Bool
goodFlags flags =
    and $ map (flip elem $ options) flags

getSrc :: [String] -> Types.Source String
getSrc filenames
    | null filenames = Types.StdIn
    | otherwise = Types.File $ head filenames

getMode :: [Char] -> Types.Mode
getMode flags
    | elem 'h' flags = Types.HelpMode
    | elem 'u' flags = Types.UnshuffleMode
    | otherwise = Types.ShuffleMode

getBase :: [Char] -> Types.Base
getBase flags
    | elem 'x' flags = Types.Hex
    | elem 'd' flags = Types.Dec
    | otherwise = Types.Dec

getOpts :: [String] -> Maybe Types.Cmds
getOpts args = if goodFlags flags
        then Just Types.Cmds
            { Types.source = getSrc filenames
            , Types.mode = getMode flags
            , Types.base = getBase flags }
        else Nothing
    where
        flags = [y | (x:xs) <- args, y <- xs, x == '-']
        filenames = [w | w@(x:xs) <- args, x /= '-']
