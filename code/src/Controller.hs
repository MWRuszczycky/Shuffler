module Controller
    ( printCommands
    ) where

import System.IO
import qualified Types as Types

printCommands :: Types.Cmds -> IO ()
printCommands cmds = do
    case Types.source cmds of
        Types.StdIn -> putStrLn "Standard input"
        Types.File fn -> putStrLn $ "File: " ++ fn
    case Types.mode cmds of
        Types.ShuffleMode -> putStrLn "Shuffle mode"
        Types.UnshuffleMode -> putStrLn "Unshuffle mode"
        Types.HelpMode -> putStrLn "Help mode"
    case Types.base cmds of
        Types.Dec -> putStrLn "Decimal base"
        Types.Hex -> putStrLn "Hex base"
    return ()
