import qualified System.Environment as Env (getArgs)
import qualified GetOpts as GetOpts
import qualified Types as Types
import qualified Controller as Ctr

main = do
    args <- Env.getArgs
    let parsedCmds = GetOpts.getOpts args
    if GetOpts.isGoodArgs parsedCmds
        then Ctr.dispatchCmd $ (\ (Types.GoodArgs c) -> c) parsedCmds
        else putStrLn $ (\ (Types.BadArgs s) -> s) parsedCmds
