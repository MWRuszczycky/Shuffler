import qualified System.Environment as Env (getArgs)
import qualified GetOpts as GetOpts
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

main = do
    args <- Env.getArgs
    let maybeCmds = GetOpts.getOpts args
    if maybeCmds == Nothing
        then putStrLn "Bad commands!"
        else printCommands $ (\ (Just c) -> c) maybeCmds
