module Controller
    ( dispatchCmd
    ) where

import System.IO
import System.Directory (doesFileExist)
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

doHelp :: Types.Cmds -> IO ()
-- ^Displays the help text to the terminal.
doHelp cmd = do
    putStrLn Model.helpStr

getInputString :: Types.Cmds -> IO String
-- ^Obtains the string to be un/shuffled from the input source
-- as an IO action.
getInputString cmd = do
    case Types.source cmd of
        Types.StdIn -> getContents
        Types.File fn -> readFile fn

doShuffle :: Types.Cmds -> IO ()
-- ^Obtains the input string, shuffles and numbers it before sending
-- the result to standard output with simple columnar printing.
doShuffle cmd = do
    toShuffleRaw <- getInputString cmd
    stdGen <- Rand.getStdGen
    let numShuffled = Model.numShuffle toShuffleRaw stdGen
        fPairs = [Model.formatPair x n (Types.base cmd) | (x,n) <- numShuffled]
        n = Model.maxLength fPairs
    mapM_ putStr . Model.addPadding 6 n $ fPairs

doUnshuffle :: Types.Cmds -> IO ()
-- ^Obtains the input string, unshuffles it and sends the result to
-- standard output.
doUnshuffle cmd = do
    toUnshuffle <- getInputString cmd
    putStrLn $ Model.unShuffle toUnshuffle

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
