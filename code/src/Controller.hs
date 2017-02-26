module Controller
    ( dispatchCmd
    ) where

import System.IO
import System.Directory (doesFileExist)
import qualified System.Random as Rand (getStdGen)
import qualified Types as Types
import qualified Model as Model

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

getInputString :: Types.Cmds -> IO String
getInputString cmd = do
    case Types.source cmd of
        Types.StdIn -> getContents
        Types.File fn -> readFile fn

doShuffle :: Types.Cmds -> IO ()
doShuffle cmd = do
    toShuffleRaw <- getInputString cmd
    let toShuffle = unwords . lines $ toShuffleRaw
    stdGen <- Rand.getStdGen
    let (shuffled, _) = Model.shuffleList toShuffle stdGen
    putStrLn shuffled

doUnshuffle :: Types.Cmds -> IO ()
doUnshuffle cmd = do
    toUnshuffle <- getInputString cmd
    putStrLn $ "unshuffling " ++ toUnshuffle

dispatchCmd :: Types.Cmds -> IO ()
dispatchCmd cmd = do
    case Types.source cmd of
        Types.StdIn -> dispatch (Types.mode cmd) cmd
        Types.File fn -> do
            fileExists <- doesFileExist fn
            if fileExists
                then dispatch (Types.mode cmd) cmd
                else putStrLn $ "File '" ++ fn ++ "' does not exist"
