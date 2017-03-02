module Model
    ( shuffleList
    , toHex
    , formatPair
    ) where

import qualified Types as Types
import qualified System.Random as Rand

---------------------------------------------------------------------
-- Functions for shuffling lists
---------------------------------------------------------------------

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

---------------------------------------------------------------------
-- Functions for formatting and extracting pairs
---------------------------------------------------------------------

formatPair :: Char -> Int -> Types.Base -> String
-- ^Formats character-number pairs for outputting the shuffled
-- sequences. The Types.Base variable is used to determine the
-- format of the sequence number.
formatPair c n b = if c == ' '
    then "( )" ++ nf
    else [c] ++ nf
        where nf = case b of
                        Types.Dec -> "-" ++ show n
                        Types.Hex -> "-" ++ toHex n
                        Types.NoBase -> ""

parsePairs :: String -> [(String, String)]
-- ^Extracts hyphen and space delimited pairs from an input string.
-- The parsing is intended to reverse the shuffle output. If there
-- are missing dashes results in default numbering from 0.
parsePairs w
    | w == [] = []
    | not $ elem '-' w = zip (words w) (map show [0..])
    | otherwise =
        let trimmed = dropWhile (== ' ') w  -- drop leading spaces
            x = takeWhile (/= '-') trimmed  -- before the dash after spaces
            xs = dropWhile (/= '-') trimmed -- dash & everything after
            d:n = takeWhile (/= ' ') xs     -- after dash before spaces
            rest = dropWhile (/= ' ') xs    -- everything else
        in (x,n):(parsePairs rest)

---------------------------------------------------------------------
-- Functions for formatting and unformatting hexadecimal numbers
---------------------------------------------------------------------

toHex :: Int -> String
-- ^Converts an Integral type from its decimal representation to its
-- hexadecimal representation as a String with most significant digit
-- first.
toHex n = reverse $ map hexFormat (getHex n 0)

getHex :: Int -> Int -> [Int]
-- ^Helper function for toHex that converts an Int type from its
-- decimal representation to its hexadecimal representation as a list
-- of Int with the most significant digit last.
getHex n p
    | n == 0 = [0]
    | div n (16^p) == 0 = []
    | otherwise = div (n - red) (16^p) : hex
    where hex = getHex n (p+1)
          red = sum $ zipWith (*) hex [16^z | z <- [(p+1)..]]

hexFormat :: Int -> Char
-- ^Helper function for toHex that converts decimal integers less
-- 16 to their hexadecimal format as Char.
hexFormat n = hexKey !! (mod n 16)
    where hexKey = ['0'..'9'] ++ ['a'..'f']
