Week 1

-- Credit card validator

{-
-- reverse a list
reversed :: [Integer] -> [Integer]
reversed [] = []
reversed (n:ms) = reverse ms ++ [n]
-}

{-- doubles the numbers on the even indices from the right - using toDigitsRev
doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' n = reverse (doubleEveryOtherLeft (reverse n))-}


-- Hanoi tower
