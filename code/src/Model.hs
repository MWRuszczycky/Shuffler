module Model
    ( numShuffle
    , unShuffle
    , formatPair
    , helpStr
    , maxLength
    , addPadding
    ) where

import qualified Types as Types
import qualified System.Random as Rand
import Data.List ( foldl' )
import Data.Char ( isSpace )

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

numShuffle :: Rand.RandomGen g => String -> g -> [(Char, Int)]
-- ^Takes a string and random seed and returns an associative array
-- with the decomposed string and the numeric position of each
-- character in the original, unshuffled string.
numShuffle str stdGen   = shuffled
    where strDecomp     = unwords . lines $ str
          numDecomp     = zip strDecomp [0..]
          (shuffled, _) = shuffleList numDecomp stdGen

sortPairs :: Ord a => [(a, String)] -> [(a, String)]
-- ^Naive quicksort for an associative array with orderable keys.
sortPairs [] = []
sortPairs (x:xs) = belowPiv ++ [x] ++ abovePiv
    where belowPiv = sortPairs [p | p <- xs, fst p <= fst x]
          abovePiv = sortPairs [p | p <- xs, fst p > fst x]

unShuffle :: String -> String
-- ^Formats and unshuffles the input string returning the formatted
-- output string.
unShuffle inStr = concat [s | (_,s) <- sorted]
    where sorted = sortPairs $ map readPair (parsePairs inStr)

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
                        Types.Dec -> "-d" ++ show n
                        Types.Hex -> "-x" ++ toHex n
                        Types.NoBase -> ""

parsePairs :: String -> [(String, String)]
-- ^Extracts hyphen and space delimited pairs from an input string.
-- The parsing is intended to reverse the shuffle output.
parsePairs w
    | w == [] = []
    | isSpace . head $ w = parsePairs . dropWhile isSpace $ w
    | otherwise =
        let x = takeWhile (/= '-') w             -- before dash after spaces
            xs = dropWhile (/= '-') w            -- dash & everything after
            d:n = takeWhile (not . isSpace) xs   -- after dash before spaces
            rest = dropWhile (not . isSpace) xs  -- everything else
        in (x,n):(parsePairs rest)

readPair :: (String, String) -> (Int, String)
-- ^Converts string pairs to numbered strings in pairs. This format
-- is used for unshuffling the components of shuffled strings.
readPair (s, w@(x:xs)) =
    case x of
        'x' -> ((makeNum Types.Hex xs), formStr s)
        'd' -> ((makeNum Types.Dec xs), formStr s)
        _   -> ((makeNum Types.Dec w), formStr s)

formStr :: String -> String
-- ^Used to read and convert special characters in the input string.
formStr s
    | s == "( )" = " "
    | otherwise = s

maxLength :: [String] -> Int
-- ^Finds the length of the longest string in a list of strings.
maxLength xs = foldl' findMax 0 xs
    where findMax n x = if length x > n
                           then length x
                           else n

addPadding :: Int -> Int -> [String] -> [String]
-- ^Pads all the strings in the input list to length p and adds
-- newlines to all n-th strings. If the string is too long for
-- padding then nothing is done. An extra space is added to separate
-- the output strings with a single space.
addPadding n p xs = [ f n p m x | (m,x) <- zip [1..] xs ]
    where pad m x = if length x > m
                       then ""
                       else replicate ( 1 + m - (length x) ) (' ')
          f linePt width wno x = if mod wno linePt == 0
                                    then pad width x ++ x ++ "\n"
                                    else pad width x ++ x

---------------------------------------------------------------------
-- Functions for formatting and unformatting hexadecimal numbers
---------------------------------------------------------------------

makeNum :: Types.Base -> String -> Int
-- ^Converts a positive integer as a string to an integer type based
-- on the base argument. This is similar to the read built in, but
-- allows different bases.
makeNum base s = makeNumHelper baseVal 0 0 $ reverse s
    where baseVal = case base of
                         Types.Dec -> 10
                         Types.Hex -> 16
                         _         -> 10

makeNumHelper :: Int -> Int -> Int -> String -> Int
-- ^Helper function for the makeNum function that converts numbers as
-- strings to positive integers.
-- Args: b is the base, p is the power and n is the result.
makeNumHelper _ _ n [] = n
makeNumHelper b p n (c:cs) = makeNumHelper b (p + 1) (n + f c * b^p) cs
    where keyMap = zip (['0'..'9'] ++ ['a'..]) [0..b]
          f c    = case lookup c keyMap of
                        Just x    -> x
                        Nothing   -> 0

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

---------------------------------------------------------------------
-- Help output string
---------------------------------------------------------------------
helpStr :: String
helpStr = unlines
    [ "\nUsage: shuffler [OPTION] [FILE]"
    , "Shuffle/unshuffle FILE or standard input to standard output.\n"
    , "Options:"
    , "   -d           shuffle with decimal numbering (default)"
    , "   -x           shuffle with hexadecimal numbering"
    , "   -n           shuffle, but do not number the output"
    , "   -u           unshuffle (requires numbered shuffle output)"
    , "   -h           display help\n"
    , "With no FILE, standard input is shuffled/unshuffled."]
