module Controller
    ( dispatchCmd
    ) where

import System.IO
import System.Directory (doesFileExist)
import Text.Printf (printf)
import qualified System.Random as Rand (getStdGen)
import qualified Types as Types
import qualified Model as Model

dispatch :: Types.Mode -> Types.Cmds -> IO ()
-- ^Routes program flow based on the mode defined defined by the
-- input options.
dispatch m
    | m == Types.HelpMode = doHelp
    | m == Types.ShuffleMode = doShuffle
    | m == Types.UnshuffleMode = doUnshuffle
    | m == Types.PrintMode = printCommands

printCommands :: Types.Cmds -> IO ()
-- ^Prints a summary of how the program will operate based on the
-- input options.
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
-- ^Displays the help text to the terminal.
doHelp cmd = do
    putStrLn "Running Help"

getInputString :: Types.Cmds -> IO String
-- ^Obtains the string to be un/shuffled from the input source
-- as an IO action.
getInputString cmd = do
    case Types.source cmd of
        Types.StdIn -> getContents
        Types.File fn -> readFile fn

doShuffle :: Types.Cmds -> IO ()
-- ^Obtains the input string, shuffles it and sends the result to
-- standard output.
doShuffle cmd = do
    toShuffleRaw <- getInputString cmd
    let toShuffle = unwords . lines $ toShuffleRaw
        numbered = zip toShuffle [0..]
    stdGen <- Rand.getStdGen
    let (shuffled, _) = Model.shuffleList numbered stdGen
    let outputStr = unwords [format x n | (x,n) <- shuffled]
            where format x n = if x == ' '
                  then "()-" ++ (show n)
                  else [x] ++ "-" ++ (show n)
    putStr outputStr

doUnshuffle :: Types.Cmds -> IO ()
-- ^Obtains the input string, unshuffles it and sends the result to
-- standard output.
doUnshuffle cmd = do
    toUnshuffle <- getInputString cmd
    putStrLn $ "unshuffling " ++ toUnshuffle

dispatchCmd :: Types.Cmds -> IO ()
-- ^Routes the program flow after checking to make sure an input
-- string is available.
dispatchCmd cmd = do
    case Types.source cmd of
        Types.StdIn -> dispatch (Types.mode cmd) cmd
        Types.File fn -> do
            fileExists <- doesFileExist fn
            if fileExists
                then dispatch (Types.mode cmd) cmd
                else putStrLn $ "File '" ++ fn ++ "' does not exist"
