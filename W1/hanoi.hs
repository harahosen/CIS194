{-# OPTIONS_GHC -Wall #-}

-- Exercise 2:
-- Hanoi tower

module Hanoi where

type Peg = String

type Move = (Peg, Peg)

-- 1. Hanoi tower with three pegs

-- Hanoi tower algorithm with three pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 source _ final = [(source,final)]
hanoi n source aux final = hanoi (n-1) source final aux ++ hanoi 1 source aux final ++ hanoi (n-1) aux source final
{-- same but using guards
 | n == 0 = []
 | n == 1 = [(source,final)]
 | otherwise = hanoi (n-1) source final aux ++ hanoi 1 source aux final ++ hanoi (n-1) aux source final
-}


-- 2. Hanoi tower with four pegs 

-- calculates the k number for the Frame-Stewart algorithm
getFSNumber :: Integer -> Integer
getFSNumber n = n - round (sqrt (2*(fromIntegral n :: Double) + 1)) + 1

-- used Frame-Stewart algorithm (see Wikipedia for details on the forumlae)
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 source _ _ final = [(source,final)]
hanoi4 n source aux aux2 final = hanoi4 (getFSNumber n) source aux final aux2 ++ hanoi (n-getFSNumber n) source aux final ++ hanoi4 (getFSNumber n) aux2 aux source final
