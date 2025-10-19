{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- Exercise 1:
-- Credit card validator 

-- gets last digit
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- gets init of a number
initDigit :: Integer -> Integer
initDigit n = div n 10

-- splits a number in a list of single digits
toDigits :: Integer -> [Integer]
toDigits n
 | n > 0 = toDigits (initDigit n) ++ [lastDigit n]
 | otherwise = []

-- splits a number in a list of single digits in reverse order
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
 | n > 0 = lastDigit n : toDigitsRev (initDigit n)
 | otherwise = []

-- doubles the numbers on the even indices from the left
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (n:m:ps) = [n,m*2] ++ doubleEveryOtherLeft ps
doubleEveryOtherLeft n = n

-- doubles the numbers on the even indices from the right - first method
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherLeft (reverse n))

-- sums all the digits inside a list
sumDigits :: [Integer] -> Integer
sumDigits (n:ms) = initDigit n + lastDigit n + sumDigits ms
sumDigits [] = 0
-- with foldr:
-- sumDigits ms = foldr (\ n -> (+) (initDigit n + lastDigit n)) 0 ms

validateNumber :: Integer -> Bool
validateNumber n
 | lastDigit n == 0 = True
 | otherwise = False

validate :: Integer -> Bool
validate n
 | validateNumber (sumDigits (doubleEveryOther (toDigits n))) = True
 | otherwise = False
