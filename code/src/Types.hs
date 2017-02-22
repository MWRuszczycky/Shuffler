module Types
    ( module Types
    ) where

data Source a = StdIn | File a deriving (Eq)

data Mode = HelpMode | ShuffleMode | UnshuffleMode deriving (Eq)

data Base = Dec | Hex deriving (Eq)

data Cmds = Cmds
    { source :: Source String
    , mode :: Mode
    , base :: Base }
    deriving (Eq)
