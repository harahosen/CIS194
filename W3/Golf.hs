{-# OPTIONS_GHC -Wall #-}

module Golf where 

import Data.List (intercalate)

-- Ex. 1: hopscotch

-- first function: we take the corrisponding index of each element in the list
takeIndices :: [a] -> [Int]
takeIndices xs = [i | (i, _) <- zip [1..] xs]

-- second fuction: we take the elements of the list that have an index that is a multiple of n
moltiplyIndex :: Int -> [a] -> [a]
moltiplyIndex 0 _ = []
moltiplyIndex n xs = [x | (i, x) <- zip [1..] xs, i `mod` n == 0]

-- composite function: applying the other functions, from a list we get a list of lists, 
-- each one containing the elements of the original list that have an index that is a multiple of index taken in consideration
-- the indices are iterating from the first to the last index of the original list
skips :: [a] -> [[a]]
skips xs = [moltiplyIndex n xs | n <- takeIndices xs]



-- Ex. 2: local maxima

-- first function: we split a list into triplets
triplets :: [a] -> [[a]]
triplets xs
  | length xs < 3 = []
  | otherwise = take 3 xs : triplets (tail xs)

-- second function: we take the local maximum of a triplet, if it exists
-- we are taking as aoutput a list of one element in order to 
-- output a void list in case there are no local maximum without excluding the case where a 0 is a local maximum
localMaximum :: (Ord a, Num a, Integral a) => [a] -> [a]
localMaximum [x, y, z]
    | y > x && y > z = [y]
    | otherwise = []
localMaximum _ = []

-- composite function: applying the other functions, from a list of integers we get a list of integers that are local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = concat (map localMaximum (triplets xs))
-- localMaxima xs = concatMap localMaximum (triplets xs) -- I don't want to use concatMap only because readability for my actual comprehension of Haskell syntax
-- localMaxima xs = [localMaximum triplet | triplet <- triplets xs] -- another alternative (not tested)



-- Ex. 3: histogram


-- first function: we count the occurrences of each digit from 0 to 9 in a list of integers
countOccurrences :: [Integer] -> [Integer]
countOccurrences xs = [cnt n xs | n <- [0..9]]
 where cnt n ys = fromIntegral (length (filter (== n) ys))

-- second function: we build a line of the histogram for a given height and maximum height
bricks :: Integer -> Integer -> String
bricks height maxHeight = replicate (fromIntegral (maxHeight - height)) ' ' ++ replicate (fromIntegral height) '*'

-- third function: we build the histogram as a list of strings, each string representing a row of the histogram
pileUp :: [String] -> Integer -> [String]
pileUp rows maxHeight = [[row !! i | row <- rows] | i <- [0..(fromIntegral maxHeight - 1)]]

-- composite function: we build the histogram from a list of integers
histogram :: [Integer] -> String
histogram xs = intercalate "\n" (pileUp columns maxHeight) ++ "\n==========\n0123456789"
 where
  counts = countOccurrences xs
  maxHeight = maximum counts
  columns = [bricks count maxHeight | count <- counts]
