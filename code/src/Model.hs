module Model
    ( shuffleList
    , toHex
    , formatPair
    ) where

import qualified Types as Types
import qualified System.Random as Rand

deletePos :: Int -> [a] -> [a]
-- ^Delete an element from a list at a specified position.
deletePos _ [] = []
deletePos n (x:xs)
    | n < 0 = x:xs
    | n == 0 = xs
    | n > 0 = x:deletePos (n-1) xs

shuffleList :: Rand.RandomGen g => [a] -> g -> ([a], g)
-- ^Randomly shuffle a list.
shuffleList [] g0 = ([], g0)
shuffleList xs gen =
    let (pos, nextGen) = Rand.randomR (0, (length xs)-1) gen
        x = xs !! pos
        (remainder, g0) = shuffleList (deletePos pos xs) nextGen
    in (x:remainder, g0)

formatPair :: (Show a, Integral a) => Char -> a -> Types.Base -> String
-- ^Formats character-number pairs for outputting the shuffled
-- sequences. The Types.Base variable is used to determine the
-- format of the sequence number.
formatPair c n b = if c == ' '
    then "( )-" ++ nf
    else [c] ++ "-" ++ nf
        where nf = case b of
                        Types.Dec -> show n
                        Types.Hex -> toHex n

toHex :: (Show a, Integral a) => a -> String
-- ^Converts an Integral type from its decimal representation to its
-- hexadecimal representation as a String with most significant digit
-- first.
toHex n = reverse $ map hexFormat (getHex n 0)

getHex :: Integral a => a -> a -> [a]
-- ^Helper function for toHex that converts an Integral type from its
-- decimal representation to its hexadecimal representation as a list
-- of integers with the most significant digit last.
getHex n p
    | n == 0 = [0]
    | div n (16^p) == 0 = []
    | otherwise = div (n - red) (16^p) : hex
    where hex = getHex n (p+1)
          red = sum $ zipWith (*) hex [16^z | z <- [(p+1)..]]

hexFormat :: (Show a, Integral a) => a -> Char
-- ^Helper function for toHex that converts decimal integers less
-- 16 to their hexadecimal format as Char.
hexFormat n
    | remainder < 10 = head . show $ remainder
    | remainder == 10 = 'a'
    | remainder == 11 = 'b'
    | remainder == 12 = 'c'
    | remainder == 13 = 'd'
    | remainder == 14 = 'e'
    | remainder == 15 = 'f'
    where remainder = mod n 16
