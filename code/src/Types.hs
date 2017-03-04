module Types
    ( module Types
    ) where

data Source a =
    StdIn
  | File a
  deriving (Eq)

data Mode =
    HelpMode
  | ShuffleMode
  | UnshuffleMode
  | PrintMode
  deriving (Eq)

data Base =
    NoBase
  | Dec
  | Hex
  deriving (Eq)

data EitherArgs a b =
    BadArgs a
  | GoodArgs b
  deriving (Eq)

data Cmds = Cmds
    { source :: Source String
    , mode :: Mode
    , base :: Base }
    deriving (Eq)
