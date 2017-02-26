module Controller
    ( dispatchCmd
    ) where

import System.IO
import System.Directory (doesFileExist)
import qualified Types as Types

dispatch :: [(Types.Mode, Types.Cmds -> IO ())]
dispatch =
    [ (Types.HelpMode, doHelp)
    , (Types.ShuffleMode, doShuffle)
    , (Types.UnshuffleMode, doShuffle)
    , (Types.PrintMode, printCommands) ]

printCommands :: Types.Cmds -> IO ()
printCommands cmd = do
    case Types.source cmd of
        Types.StdIn -> putStrLn "Standard input"
        Types.File fn -> putStrLn $ "File: " ++ fn
    case Types.mode cmd of
        Types.ShuffleMode -> putStrLn "Shuffle mode"
        Types.UnshuffleMode -> putStrLn "Unshuffle mode"
        Types.HelpMode -> putStrLn "Help mode"
        Types.PrintMode -> putStrLn "Help mode"
    case Types.base cmd of
        Types.Dec -> putStrLn "Decimal base"
        Types.Hex -> putStrLn "Hex base"
    return ()

doHelp :: Types.Cmds -> IO ()
doHelp cmd = do
    putStrLn "Running Help"

doShuffle :: Types.Cmds -> IO ()
doShuffle cmd = do
    case Types.source cmd of
        Types.StdIn -> putStrLn "un/shuffling stdin"
        Types.File fn -> do
            fileExists <- doesFileExist fn
            if fileExists
                then do
                    putStrLn $ "un/shuffling " ++ fn
                else do
                    putStrLn $ "File " ++ fn ++ "does not exist"

dispatchCmd :: Types.Cmds -> IO ()
dispatchCmd cmd = do
    let runMode = lookup (Types.mode cmd) dispatch
    case runMode of
        Nothing -> return ()
        Just f -> f cmd
