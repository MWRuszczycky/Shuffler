module Controller
    ( dispatchCmd
    ) where

import System.IO
import System.Directory (doesFileExist)
import qualified Types as Types

dispatch :: Types.Mode -> Types.Cmds -> IO ()
dispatch m
    | m == Types.HelpMode = doHelp
    | m == Types.ShuffleMode = doShuffle
    | m == Types.UnshuffleMode = doUnshuffle
    | m == Types.PrintMode = printCommands

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
    putStrLn "Running shuffle"

doUnshuffle :: Types.Cmds -> IO ()
doUnshuffle cmd = do
    putStrLn "Running unshuffle"

--routeShuffle :: Types.Cmds -> IO ()
--routeShuffle cmd = do
--    case Types.source cmd of
--        Types.StdIn -> do
--            content <- getContents
--            shufDispatch cmd $ content
--            return ()
--        Types.File fn -> do
--            fileExists <- doesFileExist fn
--            if fileExists
--                then do
--                    content <- readFile fn
--                    shufDispatch cmd $ content
--                else do
--                    putStrLn $ "File '" ++ fn ++ "' does not exist"

dispatchCmd :: Types.Cmds -> IO ()
dispatchCmd cmd = do
    case Types.source cmd of
        Types.StdIn -> dispatch (Types.mode cmd) cmd
        Types.File fn -> do
            fileExists <- doesFileExist fn
            if fileExists
                then dispatch (Types.mode cmd) cmd
                else putStrLn $ "File '" ++ fn ++ "' does not exist"
