module Model
    ( shuffleList
    ) where

import qualified Types as Types
import qualified System.Random as Rand

deletePos :: Int -> [a] -> [a]
-- ^deletePos deletes the element from a list at a specified position.
deletePos _ [] = []
deletePos n (x:xs)
    | n < 0 = x:xs
    | n == 0 = xs
    | n > 0 = x:deletePos (n-1) xs

shuffleList :: Rand.RandomGen g => [a] -> g -> ([a], g)
-- ^shuffle randomly shuffles a list
shuffleList [] g0 = ([], g0)
shuffleList xs gen =
    let (pos, nextGen) = Rand.randomR (0, (length xs)-1) gen
        x = xs !! pos
        (remainder, g0) = shuffleList (deletePos pos xs) nextGen
    in (x:remainder, g0)
