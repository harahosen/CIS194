{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- Ex. 1: Fibonacci recursive function
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fib <$> [0..]

-- Ex. 2: Fibonacci infinite list O(n)
fib2 :: Integer -> Integer
fib2 n = fst $ foldl (\(a, b) _ -> (b, a + b)) (0, 1) [1..n]

fibs2 :: [Integer]
fibs2 = fib2 <$> [0..]

-- Ex. 3: stream to list
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a strm) = a : streamToList strm

instance Show a => Show (Stream a) where
    show = show . take 32 . streamToList 

-- Ex4: stream tools
streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a strm) = Cons (f a) (streamMap f strm)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

-- Ex.5: streams gang
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleveStreams :: Stream a -> Stream a -> Stream a
interleveStreams (Cons a strm1) strm2 = Cons a (interleveStreams strm2 strm1)

ruler :: Stream Integer
-- I tried to use the functions defined before,
-- it should be the same of:
-- ruler = foldr1 interleaveStreams (streamRepeat <$> [0..])
ruler = foldr1 interleveStreams (streamToList (streamMap streamRepeat nats))

-- Ex.6: Fibonacci numbers via generating functions
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons a strm) = Cons (negate a) (negate strm)
    (+) (Cons a strm1) (Cons b strm2) = Cons (a + b) (strm1 + strm2)
    (*) (Cons a strm1) (Cons b strm2) = Cons (a * b) (streamMap (* a) strm2 + strm1 * Cons b strm2)

instance Fractional (Stream Integer) where
    (/) (Cons a strm1) (Cons b strm2) = Cons (a `div` b) ((strm1 - streamMap (* (a `div` b)) strm2) / Cons b strm2)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

-- Ex. 7: Fibonacci numbers via matrices
data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    (*) (Matrix a b c d) (Matrix e f g h) = Matrix (a * e + b * g) (a * f + b * h) (c * e + d * g) (c * f + d * h)
    fromInteger n = Matrix n 0 0 n
    negate (Matrix a b c d) = Matrix (negate a) (negate b) (negate c) (negate d)
    (+) (Matrix a b c d) (Matrix e f g h) = Matrix (a + e) (b + f) (c + g) (d + h)

fib4 :: Integer -> Integer
fib4 n = let Matrix _ _ fn _ = Matrix 1 1 1 0 ^ n in fn
-- or we can take the second element due to the properties of the Fibonacci matrix: 
-- fib4 n = let Matrix _ fn _ _ = Matrix 1 1 1 0 ^ n in fn
